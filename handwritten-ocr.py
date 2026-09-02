#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "mistralai>=2.9.4,<3",
# ]
# ///

"""Create an Emacs-friendly OCR import bundle from a handwritten PDF or image.

The script deliberately stops at a provider-neutral bundle.  Emacs can convert
note.md to Org, create the org-roam node/ID, and move the source and assets into
the user's preferred attachment layout.
"""

from __future__ import annotations

import argparse
import base64
import binascii
import hashlib
import json
import mimetypes
import os
import re
import shutil
import sys
import tempfile
import urllib.parse
import uuid
from collections.abc import Iterable
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from mistralai.client import Mistral

SCRIPT_VERSION = "1.0.0"
BUNDLE_KIND = "handwritten-ocr-import-bundle"
BUNDLE_SCHEMA = 1
DEFAULT_MODEL = "mistral-ocr-latest"

_MIME_TO_EXTENSION = {
    "application/pdf": ".pdf",
    "image/avif": ".avif",
    "image/bmp": ".bmp",
    "image/gif": ".gif",
    "image/heic": ".heic",
    "image/heif": ".heif",
    "image/jpeg": ".jpg",
    "image/png": ".png",
    "image/tiff": ".tiff",
    "image/webp": ".webp",
}

_EXTENSION_TO_MIME = {
    extension: mime_type for mime_type, extension in _MIME_TO_EXTENSION.items()
}
_EXTENSION_TO_MIME.update({".jpeg": "image/jpeg", ".tif": "image/tiff"})

# Mistral currently emits simple placeholders such as
# ![img-0.jpeg](img-0.jpeg).  This also accepts angle-bracket destinations and
# optional Markdown link titles without pretending to be a full Markdown parser.
_MARKDOWN_IMAGE_RE = re.compile(
    r"(?P<prefix>!\[[^\]\n]*\]\()"
    r"(?P<target><[^>\n]+>|[^)\s]+)"
    r"(?P<title>\s+(?:\"[^\"\n]*\"|'[^'\n]*'))?"
    r"(?P<suffix>\))"
)


class BundleError(RuntimeError):
    """A concise, user-facing failure."""


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Transcribe a local handwritten PDF/image with Mistral OCR and "
            "write note.md, manifest.json, the original, and extracted assets."
        ),
        epilog=(
            "Example:\n"
            "  export MISTRAL_API_KEY=...\n"
            "  uv run handwritten-ocr.py notebook-page.pdf\n\n"
            "On success, stdout contains one JSON object for easy Emacs parsing; "
            "progress and errors go to stderr."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("input", type=Path, help="PDF or image to transcribe")
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="bundle directory (default: INPUT_STEM.ocr beside the input)",
    )
    parser.add_argument(
        "--provider",
        choices=("mistral",),
        default="mistral",
        help="OCR provider; retained as a stable interface for future backends",
    )
    parser.add_argument(
        "--model",
        default=DEFAULT_MODEL,
        help=f"Mistral OCR model (default: {DEFAULT_MODEL})",
    )
    parser.add_argument(
        "--api-key-env",
        default="MISTRAL_API_KEY",
        metavar="NAME",
        help="environment variable containing the API key (default: MISTRAL_API_KEY)",
    )
    parser.add_argument(
        "--confidence",
        choices=("none", "page", "block", "word"),
        default="block",
        help="confidence detail retained in ocr.json (default: block)",
    )
    parser.add_argument(
        "--title",
        help="suggested title for manifest.json (default: derived from filename)",
    )
    parser.add_argument(
        "--timeout",
        type=positive_int,
        default=600,
        metavar="SECONDS",
        help="OCR request timeout (default: 600)",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="explicitly re-run OCR and replace an existing recognized bundle",
    )
    parser.add_argument(
        "--version", action="version", version=f"%(prog)s {SCRIPT_VERSION}"
    )
    return parser.parse_args(argv)


def positive_int(value: str) -> int:
    try:
        parsed = int(value)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("must be an integer") from exc
    if parsed <= 0:
        raise argparse.ArgumentTypeError("must be greater than zero")
    return parsed


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sniff_mime_type(path: Path) -> str:
    with path.open("rb") as source:
        header = source.read(32)

    if header.startswith(b"%PDF-"):
        return "application/pdf"
    if header.startswith(b"\x89PNG\r\n\x1a\n"):
        return "image/png"
    if header.startswith(b"\xff\xd8\xff"):
        return "image/jpeg"
    if header[:6] in (b"GIF87a", b"GIF89a"):
        return "image/gif"
    if header.startswith(b"BM"):
        return "image/bmp"
    if header.startswith((b"II*\x00", b"MM\x00*")):
        return "image/tiff"
    if len(header) >= 12 and header[:4] == b"RIFF" and header[8:12] == b"WEBP":
        return "image/webp"
    if len(header) >= 12 and header[4:8] == b"ftyp":
        brand = header[8:12]
        if brand in (b"avif", b"avis"):
            return "image/avif"
        if brand in (b"heic", b"heix", b"hevc", b"hevx"):
            return "image/heic"
        if brand in (b"mif1", b"msf1"):
            return "image/heif"

    guessed, _ = mimetypes.guess_type(path.name)
    if guessed == "application/pdf" or (guessed and guessed.startswith("image/")):
        return guessed.lower()
    raise BundleError(
        "input must be a PDF or a recognizable image (for example PNG, JPEG, "
        "AVIF, or WebP)"
    )


def source_extension(path: Path, mime_type: str) -> str:
    suffix = path.suffix.lower()
    if suffix in _EXTENSION_TO_MIME and _EXTENSION_TO_MIME[suffix] == mime_type:
        return suffix
    return _MIME_TO_EXTENSION.get(mime_type, suffix or ".bin")


def suggested_title(path: Path) -> str:
    stem = path.stem.strip()
    dated = re.match(r"^\d{4}-\d{2}-\d{2}[-_ ]+(?P<title>.+)$", stem)
    if dated:
        stem = dated.group("title")
    title = re.sub(r"[-_]+", " ", stem)
    title = re.sub(r"\s+", " ", title).strip()
    return title or "Handwritten note"


def read_existing_manifest(output: Path) -> dict[str, Any] | None:
    manifest_path = output / "manifest.json"
    if not manifest_path.is_file():
        return None
    try:
        value = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return None
    if not isinstance(value, dict):
        return None
    if value.get("kind") != BUNDLE_KIND or value.get("schema") != BUNDLE_SCHEMA:
        return None
    return value


def resolve_paths(input_arg: Path, output_arg: Path | None) -> tuple[Path, Path]:
    input_path = input_arg.expanduser().resolve()
    if not input_path.exists():
        raise BundleError(f"input does not exist: {input_path}")
    if not input_path.is_file():
        raise BundleError(f"input is not a regular file: {input_path}")

    default_output = input_path.with_name(f"{input_path.stem}.ocr")
    output = (output_arg or default_output).expanduser().resolve()
    if output == Path(output.anchor):
        raise BundleError("refusing to use a filesystem root as the output directory")
    if output == input_path or output in input_path.parents:
        raise BundleError("the input file cannot be inside the output directory")
    if output.exists() and not output.is_dir():
        raise BundleError(f"output exists and is not a directory: {output}")
    return input_path, output


def result_object(
    status: str, output: Path, manifest: dict[str, Any]
) -> dict[str, Any]:
    return {
        "status": status,
        "bundle": str(output),
        "manifest": str(output / "manifest.json"),
        "source_sha256": manifest["source_sha256"],
    }


def encode_document(path: Path, mime_type: str) -> dict[str, str]:
    encoded = base64.b64encode(path.read_bytes()).decode("ascii")
    data_url = f"data:{mime_type};base64,{encoded}"
    if mime_type == "application/pdf":
        return {"type": "document_url", "document_url": data_url}
    return {"type": "image_url", "image_url": data_url}


def call_mistral_ocr(
    *,
    path: Path,
    mime_type: str,
    api_key: str,
    model: str,
    confidence: str,
    timeout_seconds: int,
) -> Any:
    request: dict[str, Any] = {
        "model": model,
        "document": encode_document(path, mime_type),
        "include_image_base64": True,
        "include_blocks": confidence == "block",
        "timeout_ms": timeout_seconds * 1000,
    }
    if confidence != "none":
        request["confidence_scores_granularity"] = confidence

    try:
        with Mistral(api_key=api_key) as client:
            return client.ocr.process(**request)
    except Exception as exc:
        raise BundleError(f"Mistral OCR request failed: {exc}") from exc


def object_get(value: Any, name: str, default: Any = None) -> Any:
    if isinstance(value, dict):
        return value.get(name, default)
    return getattr(value, name, default)


def response_to_jsonable(response: Any) -> dict[str, Any]:
    if hasattr(response, "model_dump"):
        value = response.model_dump(mode="json", exclude_none=True)
    elif hasattr(response, "dict"):
        value = response.dict(exclude_none=True)
    elif isinstance(response, dict):
        value = response
    else:
        raise BundleError("the OCR SDK returned an unsupported response object")
    if not isinstance(value, dict):
        raise BundleError("the OCR SDK response was not a JSON object")
    return value


def omit_base64_images(value: Any) -> Any:
    if isinstance(value, dict):
        return {
            key: omit_base64_images(item)
            for key, item in value.items()
            if key != "image_base64"
        }
    if isinstance(value, list):
        return [omit_base64_images(item) for item in value]
    return value


def decode_image_base64(value: Any, source_id: str) -> tuple[bytes, str | None]:
    if not isinstance(value, str) or not value.strip():
        raise BundleError(
            f"OCR returned extracted image {source_id!r} without image data"
        )

    encoded = value.strip()
    hinted_mime: str | None = None
    if encoded.startswith("data:"):
        try:
            header, encoded = encoded.split(",", 1)
        except ValueError as exc:
            raise BundleError(
                f"invalid data URL for extracted image {source_id!r}"
            ) from exc
        match = re.fullmatch(r"data:([^;,]+);base64", header, flags=re.IGNORECASE)
        if not match:
            raise BundleError(f"unsupported data URL for extracted image {source_id!r}")
        hinted_mime = match.group(1).lower()

    compact = "".join(encoded.split())
    try:
        data = base64.b64decode(compact, validate=True)
    except (ValueError, binascii.Error) as exc:
        raise BundleError(f"invalid base64 for extracted image {source_id!r}") from exc
    if not data:
        raise BundleError(f"extracted image {source_id!r} was empty")
    return data, hinted_mime


def image_mime_type(data: bytes, source_id: str, hinted_mime: str | None) -> str:
    if hinted_mime in _MIME_TO_EXTENSION and hinted_mime != "application/pdf":
        return hinted_mime

    if data.startswith(b"\x89PNG\r\n\x1a\n"):
        return "image/png"
    if data.startswith(b"\xff\xd8\xff"):
        return "image/jpeg"
    if data[:6] in (b"GIF87a", b"GIF89a"):
        return "image/gif"
    if data.startswith(b"BM"):
        return "image/bmp"
    if data.startswith((b"II*\x00", b"MM\x00*")):
        return "image/tiff"
    if len(data) >= 12 and data[:4] == b"RIFF" and data[8:12] == b"WEBP":
        return "image/webp"
    if len(data) >= 12 and data[4:8] == b"ftyp":
        if data[8:12] in (b"avif", b"avis"):
            return "image/avif"
        if data[8:12] in (b"heic", b"heix", b"hevc", b"hevx"):
            return "image/heic"
        if data[8:12] in (b"mif1", b"msf1"):
            return "image/heif"

    suffix_mime = _EXTENSION_TO_MIME.get(Path(source_id).suffix.lower())
    if suffix_mime and suffix_mime != "application/pdf":
        return suffix_mime
    raise BundleError(
        f"could not determine the format of extracted image {source_id!r}"
    )


def rewrite_image_links(
    markdown: str, replacements: dict[str, str]
) -> tuple[str, set[str]]:
    referenced: set[str] = set()

    def replace(match: re.Match[str]) -> str:
        raw_target = match.group("target")
        target = raw_target[1:-1] if raw_target.startswith("<") else raw_target
        target = urllib.parse.unquote(target)
        candidates = (target, target.removeprefix("./"))
        replacement = next(
            (
                replacements[candidate]
                for candidate in candidates
                if candidate in replacements
            ),
            None,
        )
        if replacement is None:
            return match.group(0)
        referenced.add(replacement)
        return (
            f"{match.group('prefix')}{replacement}"
            f"{match.group('title') or ''}{match.group('suffix')}"
        )

    return _MARKDOWN_IMAGE_RE.sub(replace, markdown), referenced


def page_sort_key(page: Any, fallback: int) -> tuple[int, int]:
    index = object_get(page, "index", fallback)
    return (index if isinstance(index, int) else fallback, fallback)


def page_number(index: Any, fallback: int) -> int:
    return index + 1 if isinstance(index, int) else fallback + 1


def bbox_for(image: Any) -> dict[str, int | None]:
    return {
        key: object_get(image, key)
        for key in (
            "top_left_x",
            "top_left_y",
            "bottom_right_x",
            "bottom_right_y",
        )
    }


def write_json(path: Path, value: Any) -> None:
    path.write_text(
        json.dumps(value, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


def build_transcription(
    response: Any, bundle_dir: Path
) -> tuple[str, list[dict[str, Any]], list[str]]:
    pages_value = object_get(response, "pages")
    if not isinstance(pages_value, Iterable) or isinstance(pages_value, (str, bytes)):
        raise BundleError("OCR response did not contain pages")
    pages = list(pages_value)
    if not pages:
        raise BundleError("OCR response contained no pages")
    pages = [
        page
        for _, page in sorted(
            enumerate(pages), key=lambda pair: page_sort_key(pair[1], pair[0])
        )
    ]

    assets_dir = bundle_dir / "assets"
    assets_dir.mkdir()
    asset_records: list[dict[str, Any]] = []
    warnings: list[str] = []
    note_pages: list[str] = []
    asset_counter = 0

    for page_ordinal, page in enumerate(pages):
        index = object_get(page, "index", page_ordinal)
        markdown_value = object_get(page, "markdown", "")
        markdown = (
            markdown_value if isinstance(markdown_value, str) else str(markdown_value)
        )
        replacements: dict[str, str] = {}
        page_asset_paths: list[str] = []
        images_value = object_get(page, "images", []) or []

        for image_ordinal, image in enumerate(images_value):
            source_id_value = object_get(image, "id")
            source_id = (
                str(source_id_value)
                if source_id_value
                else f"page-{page_number(index, page_ordinal)}-image-{image_ordinal + 1}"
            )
            if source_id in replacements:
                raise BundleError(
                    f"OCR returned duplicate image id {source_id!r} on page "
                    f"{page_number(index, page_ordinal)}"
                )
            image_data, hinted_mime = decode_image_base64(
                object_get(image, "image_base64"), source_id
            )
            mime_type = image_mime_type(image_data, source_id, hinted_mime)
            asset_counter += 1
            filename = f"figure-{asset_counter:03d}{_MIME_TO_EXTENSION[mime_type]}"
            relative_path = f"assets/{filename}"
            (assets_dir / filename).write_bytes(image_data)
            replacements[source_id] = relative_path
            page_asset_paths.append(relative_path)
            asset_records.append(
                {
                    "path": relative_path,
                    "source_id": source_id,
                    "page_index": index,
                    "mime_type": mime_type,
                    "size_bytes": len(image_data),
                    "sha256": sha256_bytes(image_data),
                    "bbox": bbox_for(image),
                }
            )

        markdown, referenced = rewrite_image_links(markdown, replacements)
        unplaced = [path for path in page_asset_paths if path not in referenced]
        if unplaced:
            warnings.append(
                f"Page {page_number(index, page_ordinal)} had {len(unplaced)} extracted "
                "visual(s) without Markdown placeholders; appended them to the page."
            )
            suffix = "\n\n".join(
                "<!-- OCR extracted this visual but did not place it in the text. -->\n"
                f"![]({path})"
                for path in unplaced
            )
            markdown = (
                f"{markdown.rstrip()}\n\n{suffix}" if markdown.strip() else suffix
            )

        marker = f"<!-- OCR page {page_number(index, page_ordinal)} -->"
        body = markdown.strip()
        note_pages.append(f"{marker}\n\n{body}" if body else marker)

    return "\n\n".join(note_pages).rstrip() + "\n", asset_records, warnings


def build_bundle(
    *,
    response: Any,
    bundle_dir: Path,
    input_path: Path,
    input_mime: str,
    source_hash: str,
    provider: str,
    model_requested: str,
    confidence: str,
    title: str,
) -> dict[str, Any]:
    original_name = f"original{source_extension(input_path, input_mime)}"
    shutil.copy2(input_path, bundle_dir / original_name)

    note, assets, warnings = build_transcription(response, bundle_dir)
    (bundle_dir / "note.md").write_text(note, encoding="utf-8")

    response_json = omit_base64_images(response_to_jsonable(response))
    write_json(bundle_dir / "ocr.json", response_json)
    model_reported = object_get(response, "model") or model_requested
    usage = response_json.get("usage_info")
    manifest: dict[str, Any] = {
        "schema": BUNDLE_SCHEMA,
        "kind": BUNDLE_KIND,
        "created_at": datetime.now(timezone.utc)
        .isoformat(timespec="seconds")
        .replace("+00:00", "Z"),
        "script_version": SCRIPT_VERSION,
        "source_sha256": source_hash,
        "source_filename": input_path.name,
        "source_mime_type": input_mime,
        "source_size_bytes": input_path.stat().st_size,
        "source_path": original_name,
        "provider": provider,
        "model_requested": model_requested,
        "model": str(model_reported),
        "confidence_scores_granularity": None if confidence == "none" else confidence,
        "suggested_title": title,
        "note_path": "note.md",
        "ocr_response_path": "ocr.json",
        "page_count": len(object_get(response, "pages", [])),
        "assets": assets,
        "warnings": warnings,
    }
    if usage is not None:
        manifest["usage"] = usage
    write_json(bundle_dir / "manifest.json", manifest)
    return manifest


def install_bundle(temp_dir: Path, output: Path, replacing: bool) -> str | None:
    if not replacing:
        os.replace(temp_dir, output)
        return None

    backup = output.with_name(f".{output.name}.backup-{uuid.uuid4().hex}")
    os.replace(output, backup)
    try:
        os.replace(temp_dir, output)
    except Exception:
        os.replace(backup, output)
        raise
    try:
        shutil.rmtree(backup)
    except OSError as exc:
        return f"new bundle installed, but old backup remains at {backup}: {exc}"
    return None


def run(args: argparse.Namespace) -> dict[str, Any]:
    input_path, output = resolve_paths(args.input, args.output)
    input_mime = sniff_mime_type(input_path)
    source_hash = sha256_file(input_path)

    existing_manifest: dict[str, Any] | None = None
    if output.exists():
        existing_manifest = read_existing_manifest(output)
        if existing_manifest is None:
            raise BundleError(
                "refusing to overwrite an existing directory that is not a recognized "
                f"OCR bundle: {output}"
            )
        if not args.force and existing_manifest.get("source_sha256") == source_hash:
            return result_object("unchanged", output, existing_manifest)
        if not args.force:
            raise BundleError(
                f"output already contains a bundle for another source: {output}; "
                "choose another --output or use --force"
            )

    api_key = os.environ.get(args.api_key_env)
    if not api_key:
        raise BundleError(
            f"API key environment variable {args.api_key_env!r} is not set"
        )

    title = args.title.strip() if args.title else suggested_title(input_path)
    if not title:
        raise BundleError("--title cannot be empty")

    print(
        f"Transcribing {input_path.name} with {args.provider}/{args.model} ...",
        file=sys.stderr,
    )
    response = call_mistral_ocr(
        path=input_path,
        mime_type=input_mime,
        api_key=api_key,
        model=args.model,
        confidence=args.confidence,
        timeout_seconds=args.timeout,
    )

    output.parent.mkdir(parents=True, exist_ok=True)
    temp_dir = Path(
        tempfile.mkdtemp(prefix=f".{output.name}.tmp-", dir=str(output.parent))
    )
    try:
        manifest = build_bundle(
            response=response,
            bundle_dir=temp_dir,
            input_path=input_path,
            input_mime=input_mime,
            source_hash=source_hash,
            provider=args.provider,
            model_requested=args.model,
            confidence=args.confidence,
            title=title,
        )
        cleanup_warning = install_bundle(
            temp_dir, output, replacing=existing_manifest is not None
        )
    except Exception:
        if temp_dir.exists():
            shutil.rmtree(temp_dir, ignore_errors=True)
        raise

    if cleanup_warning:
        print(f"warning: {cleanup_warning}", file=sys.stderr)
    status = "replaced" if existing_manifest is not None else "created"
    return result_object(status, output, manifest)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        result = run(args)
    except BundleError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print("error: interrupted", file=sys.stderr)
        return 130
    print(json.dumps(result, ensure_ascii=False, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
