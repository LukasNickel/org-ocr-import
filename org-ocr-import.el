;;; org-ocr-import.el --- Run OCR on pdf files and create new org-roam nodes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Lukas Nickel
;;
;; Author: Lukas Nickel <l.nickel@ianus-simulation.de>
;; Maintainer: Lukas Nickel <l.nickel@ianus-simulation.de>
;; Created: September 02, 2026
;; Modified: September 02, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/lnickel/org-ocr-import
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; End-to-end workflow for handwritten notes:
;;
;;   vector PDF --Ghostscript--> 300-DPI raster PDF
;;              --Mistral OCR--> OCR bundle
;;              --Pandoc/Org-->  new org-roam file or subtree in current file
;;
;; The Mistral API key is retrieved through `auth-source'.  On KDE, this can
;; use KWallet through the Freedesktop Secret Service backend.  The key is
;; injected only into the OCR subprocess environment; it is not added to the
;; global Emacs environment or passed on the command line.
;;
;; Main interactive commands:
;;
;;   M-x org-ocr-import-rasterize
;;   M-x org-ocr-import-run
;;   M-x org-ocr-import-new
;;   M-x org-ocr-import-here
;;   M-x org-ocr-import-process-new
;;   M-x org-ocr-import-process-here
;;
;; A prefix argument to the OCR/process commands passes --force to the Python
;; script, allowing replacement of an existing recognized OCR bundle.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-attach)
(require 'org-element)
(require 'org-id)
(require 'org-roam)
(require 'seq)
(require 'subr-x)

(defgroup org-ocr-import nil
  "OCR handwritten notes and import them into Org-roam."
  :group 'org-roam
  :prefix "org-ocr-import-")

(defcustom org-ocr-import-directory "handwritten"
  "Destination below `org-roam-directory' for newly created OCR notes.
Set this to nil or the empty string to create them directly in
`org-roam-directory'."
  :type '(choice (const :tag "Org-roam root" nil) directory)
  :group 'org-ocr-import)

(defcustom org-ocr-import-tags '("ocr")
  "Tags added to imported OCR notes.
For new files these become file tags.  For imports into an existing file,
they become tags on the inserted OCR heading."
  :type '(repeat string)
  :group 'org-ocr-import)

(defcustom org-ocr-import-python-script
  (expand-file-name
   "handwritten-ocr.py"
   (file-name-directory
    (or load-file-name buffer-file-name)))
  "Path to the Python script used for handwritten OCR."
  :type 'file
  :group 'handwritten-ocr)

(defcustom org-ocr-import-uv-program "uv"
  "uv executable used to run `org-ocr-import-python-script'."
  :type 'string
  :group 'org-ocr-import)

(defcustom org-ocr-import-ghostscript-program "gs"
  "Ghostscript executable used for rasterization."
  :type 'string
  :group 'org-ocr-import)

(defcustom org-ocr-import-raster-dpi 300
  "Effective DPI for full-page rasterization before OCR."
  :type 'integer
  :group 'org-ocr-import)

(defcustom org-ocr-import-raster-downscale-factor 2
  "Ghostscript supersampling/downscale factor.
The page is rendered at DPI times this factor and then downscaled by the
same factor."
  :type 'integer
  :group 'org-ocr-import)

(defcustom org-ocr-import-auth-sources nil
  "Optional `auth-sources' override used for the Mistral credential.
When nil, use the user's normal `auth-sources'.  For a specific KWallet
Secret Service collection, a value such as '(\"secrets:kdewallet\") can be
used."
  :type '(choice (const :tag "Use global auth-sources" nil)
          (repeat string))
  :group 'org-ocr-import)

(defcustom org-ocr-import-auth-host "mistral-ocr"
  "Auth-source host used to locate the Mistral API key."
  :type 'string
  :group 'org-ocr-import)

(defcustom org-ocr-import-auth-user "api"
  "Auth-source user used to locate the Mistral API key.
Set to nil to search only by host."
  :type '(choice (const :tag "Do not constrain user" nil) string)
  :group 'org-ocr-import)

(defcustom org-ocr-import-pandoc-program "pandoc"
  "Pandoc executable used for Markdown-to-Org conversion."
  :type 'string
  :group 'org-ocr-import)

(defcustom org-ocr-import-sync-before-import t
  "When non-nil, synchronize the Org-roam database before duplicate checks."
  :type 'boolean
  :group 'org-ocr-import)

(defcustom org-ocr-import-here-require-roam t
  "When non-nil, `org-ocr-import-here' requires an Org-roam file."
  :type 'boolean
  :group 'org-ocr-import)

(defconst org-ocr-import--bundle-kind "handwritten-ocr-import-bundle")
(defconst org-ocr-import--bundle-schema 1)

(defun org-ocr-import--get (key object)
  "Get string KEY from JSON alist OBJECT."
  (alist-get key object nil nil #'string=))

(defun org-ocr-import--required-string (key manifest)
  "Return required string KEY from MANIFEST or signal a user error."
  (let ((value (org-ocr-import--get key manifest)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (user-error "Invalid manifest: %s must be a non-empty string" key))
    value))

(defun org-ocr-import--one-line (value)
  "Convert VALUE to a single line suitable for an Org property."
  (string-trim
   (replace-regexp-in-string "[\n\r]+" " " (format "%s" (or value "")))))

(defun org-ocr-import--read-manifest (bundle-directory)
  "Read and validate manifest.json in BUNDLE-DIRECTORY."
  (let ((manifest-file (expand-file-name "manifest.json" bundle-directory)))
    (unless (file-regular-p manifest-file)
      (user-error "No manifest.json in %s" bundle-directory))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents manifest-file)
          (let ((manifest
                 (json-parse-buffer
                  :object-type 'alist
                  :array-type 'list
                  :null-object nil
                  :false-object nil)))
            (unless (equal (org-ocr-import--get "kind" manifest)
                           org-ocr-import--bundle-kind)
              (user-error "Not a handwritten OCR import bundle: %s"
                          bundle-directory))
            (unless (equal (org-ocr-import--get "schema" manifest)
                           org-ocr-import--bundle-schema)
              (user-error "Unsupported OCR bundle schema: %S"
                          (org-ocr-import--get "schema" manifest)))
            manifest))
      (json-parse-error
       (user-error "Could not parse %s: %s"
                   manifest-file (error-message-string err))))))

(defun org-ocr-import--bundle-file (bundle-directory relative-path)
  "Resolve RELATIVE-PATH safely inside BUNDLE-DIRECTORY."
  (unless (and (stringp relative-path)
               (not (string-empty-p relative-path))
               (not (file-name-absolute-p relative-path)))
    (user-error "Invalid bundle-relative path: %S" relative-path))
  (let ((path (expand-file-name relative-path bundle-directory)))
    (unless (file-in-directory-p path bundle-directory)
      (user-error "Bundle path escapes its directory: %s" relative-path))
    (unless (file-regular-p path)
      (user-error "Bundle file is missing: %s" path))
    path))

(defun org-ocr-import--sha256-file (file)
  "Return the SHA-256 digest of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun org-ocr-import--verify-hash (file expected description)
  "Verify FILE against EXPECTED SHA-256 for DESCRIPTION."
  (unless (and (stringp expected)
               (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" expected))
    (user-error "Invalid SHA-256 for %s in manifest" description))
  (unless (string-equal (downcase expected)
                        (org-ocr-import--sha256-file file))
    (user-error "SHA-256 mismatch for %s: %s" description file)))

(defun org-ocr-import--collect-assets (bundle-directory manifest)
  "Return validated asset records from MANIFEST in BUNDLE-DIRECTORY.
Each returned item is (RELATIVE-PATH ABSOLUTE-PATH BASENAME)."
  (let ((assets (org-ocr-import--get "assets" manifest))
        records
        basenames)
    (unless (or (null assets) (listp assets))
      (user-error "Invalid manifest: assets must be an array"))
    (dolist (asset assets)
      (unless (listp asset)
        (user-error "Invalid asset record in manifest"))
      (let* ((relative (org-ocr-import--required-string "path" asset))
             (absolute (org-ocr-import--bundle-file bundle-directory relative))
             (basename (file-name-nondirectory absolute))
             (digest (org-ocr-import--required-string "sha256" asset)))
        (when (member basename basenames)
          (user-error "Two assets have the same basename: %s" basename))
        (org-ocr-import--verify-hash absolute digest relative)
        (push basename basenames)
        (push (list relative absolute basename) records)))
    (nreverse records)))

(defun org-ocr-import--markdown-attachment-links (markdown assets)
  "Rewrite asset targets in MARKDOWN using validated ASSETS."
  (dolist (asset assets markdown)
    (let ((relative (nth 0 asset))
          (basename (nth 2 asset)))
      (setq markdown
            (replace-regexp-in-string
             (regexp-quote (format "(%s)" relative))
             (format "(attachment:%s)" basename)
             markdown t t))
      (setq markdown
            (replace-regexp-in-string
             (regexp-quote (format "(<%s>)" relative))
             (format "(attachment:%s)" basename)
             markdown t t)))))

(defun org-ocr-import--verify-asset-links (org-body assets)
  "Ensure every item in ASSETS has an attachment link in ORG-BODY."
  (dolist (asset assets)
    (let* ((basename (nth 2 asset))
           (link (format "[[attachment:%s]]" basename)))
      (unless (string-match-p (regexp-quote link) org-body)
        (error "Pandoc output omitted the attachment link for %s" basename)))))

(defun org-ocr-import--pandoc (markdown heading-shift)
  "Convert MARKDOWN to Org, shifting headings by HEADING-SHIFT levels."
  (let ((program (executable-find org-ocr-import-pandoc-program)))
    (unless program
      (user-error "Pandoc executable not found: %s"
                  org-ocr-import-pandoc-program))
    (with-temp-buffer
      (insert markdown)
      (let ((coding-system-for-read 'utf-8-unix)
            (coding-system-for-write 'utf-8-unix)
            (status
             (call-process-region
              (point-min) (point-max)
              program t t nil
              "-f" "markdown" "-t" "org" "--wrap=none"
              (format "--shift-heading-level-by=%d" heading-shift))))
        (unless (and (integerp status) (zerop status))
          (error "Pandoc failed with status %S" status))
        (string-trim-right (buffer-string))))))

(defun org-ocr-import--slug (title)
  "Return a filesystem-friendly slug for TITLE."
  (let ((slug (downcase title)))
    (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
    (setq slug (string-trim slug "-+" "-+"))
    (when (> (length slug) 80)
      (setq slug (substring slug 0 80)))
    (if (string-empty-p slug) "handwritten-note" slug)))

(defun org-ocr-import--timestamp (created-at)
  "Turn CREATED-AT into a compact UTC timestamp, with a safe fallback."
  (condition-case nil
      (format-time-string "%Y%m%d%H%M%S" (date-to-time created-at) t)
    (error (format-time-string "%Y%m%d%H%M%S" nil t))))

(defun org-ocr-import--destination (title created-at source-hash)
  "Return a new Org file path for TITLE, CREATED-AT, and SOURCE-HASH."
  (unless (and (boundp 'org-roam-directory) org-roam-directory)
    (user-error "Set org-roam-directory before importing"))
  (let* ((roam-root (file-name-as-directory
                     (expand-file-name org-roam-directory)))
         (subdirectory (or org-ocr-import-directory ""))
         (destination-directory
          (file-name-as-directory
           (expand-file-name subdirectory roam-root)))
         (filename
          (format "%s-%s-%s.org"
                  (org-ocr-import--timestamp created-at)
                  (org-ocr-import--slug title)
                  (substring source-hash 0 8)))
         (destination (expand-file-name filename destination-directory)))
    (unless (string-prefix-p roam-root destination-directory)
      (user-error "Import destination must be inside org-roam-directory"))
    (make-directory destination-directory t)
    (when (file-exists-p destination)
      (user-error "Destination already exists: %s" destination))
    destination))

(defun org-ocr-import--tag (tag)
  "Normalize TAG for Org."
  (let ((value (replace-regexp-in-string
                "[^[:alnum:]_@#%]+" "_" (string-trim tag))))
    (unless (string-empty-p value) value)))

(defun org-ocr-import--tags ()
  "Return normalized `org-ocr-import-tags'."
  (delq nil (mapcar #'org-ocr-import--tag org-ocr-import-tags)))

(defun org-ocr-import--filetags-line ()
  "Return a #+filetags line, or nil if no tags are configured."
  (let ((tags (org-ocr-import--tags)))
    (when tags
      (format "#+filetags: :%s:\n" (mapconcat #'identity tags ":")))))

(defun org-ocr-import--heading-tags ()
  "Return an Org heading tag suffix, or the empty string."
  (let ((tags (org-ocr-import--tags)))
    (if tags
        (format " :%s:" (mapconcat #'identity tags ":"))
      "")))

(defun org-ocr-import--properties (manifest id)
  "Build the OCR property drawer for MANIFEST and node ID."
  (let ((source-hash
         (org-ocr-import--required-string "source_sha256" manifest))
        (source-filename
         (org-ocr-import--required-string "source_filename" manifest))
        (source-mime (org-ocr-import--get "source_mime_type" manifest))
        (provider (org-ocr-import--required-string "provider" manifest))
        (model (org-ocr-import--required-string "model" manifest))
        (ocr-created-at
         (org-ocr-import--required-string "created_at" manifest))
        (page-count (org-ocr-import--get "page_count" manifest))
        (asset-count (length (org-ocr-import--get "assets" manifest))))
    (concat
     ":PROPERTIES:\n"
     (format ":ID: %s\n" id)
     (format ":OCR_SOURCE_SHA256: %s\n" source-hash)
     (format ":OCR_SOURCE_FILENAME: %s\n"
             (org-ocr-import--one-line source-filename))
     (when source-mime
       (format ":OCR_SOURCE_MIME: %s\n"
               (org-ocr-import--one-line source-mime)))
     (format ":OCR_PROVIDER: %s\n" (org-ocr-import--one-line provider))
     (format ":OCR_MODEL: %s\n" (org-ocr-import--one-line model))
     (format ":OCR_CREATED_AT: %s\n"
             (org-ocr-import--one-line ocr-created-at))
     (format ":OCR_IMPORTED_AT: %s\n" (format-time-string "%FT%TZ" nil t))
     (format ":OCR_PAGE_COUNT: %s\n" (or page-count ""))
     (format ":OCR_ASSET_COUNT: %d\n" asset-count)
     (format ":OCR_BUNDLE_SCHEMA: %d\n" org-ocr-import--bundle-schema)
     ":END:\n")))

(defun org-ocr-import--subtree-text
    (title level id manifest source-basename org-body &optional heading-tags)
  "Build an OCR subtree rooted at LEVEL."
  (let ((stars (make-string level ?*))
        (child-stars (make-string (1+ level) ?*)))
    (concat
     (format "%s %s%s\n" stars (org-ocr-import--one-line title)
             (or heading-tags ""))
     (org-ocr-import--properties manifest id)
     "\n"
     (format "%s Source\n\n" child-stars)
     (format "[[attachment:%s][Original scan]]\n\n" source-basename)
     (format "%s Transcription\n\n" child-stars)
     org-body
     "\n")))

(defun org-ocr-import--existing-node (source-hash)
  "Return the Org-roam node already carrying SOURCE-HASH, if any."
  (seq-find
   (lambda (node)
     (equal source-hash
            (cdr (assoc-string
                  "OCR_SOURCE_SHA256"
                  (org-roam-node-properties node)
                  t))))
   (org-roam-node-list)))

(defun org-ocr-import--attachment-headings-in-region (begin end)
  "Return heading positions containing attachment links between BEGIN and END."
  (save-restriction
    (narrow-to-region begin end)
    (delete-dups
     (delq
      nil
      (org-element-map
          (org-element-parse-buffer) 'link
        (lambda (link)
          (when (string-equal (org-element-property :type link) "attachment")
            (save-excursion
              (goto-char (org-element-property :begin link))
              (condition-case nil
                  (progn
                    (org-back-to-heading t)
                    (point))
                (error nil))))))))))

(defun org-ocr-import--set-local-attachment-directories
    (root-marker subtree-end attachment-directory)
  "Make nested attachment links resolve to ATTACHMENT-DIRECTORY.
ROOT-MARKER identifies the heading owning the ID-based directory.  Only
headings inside ROOT-MARKER..SUBTREE-END are modified."
  (let* ((relative-directory
          (directory-file-name
           (file-relative-name attachment-directory
                               (file-name-directory buffer-file-name))))
         (positions
          (org-ocr-import--attachment-headings-in-region
           (marker-position root-marker) (marker-position subtree-end))))
    ;; Work bottom-up so inserting property drawers cannot invalidate positions.
    (dolist (position (sort positions #'>))
      (unless (= position (marker-position root-marker))
        (goto-char position)
        (org-entry-put nil "DIR" relative-directory)))))

(defun org-ocr-import--copy-attachments (files directory)
  "Copy FILES into attachment DIRECTORY without overwriting anything."
  (dolist (file files)
    (let ((destination
           (expand-file-name (file-name-nondirectory file) directory)))
      (when (file-exists-p destination)
        (error "Attachment already exists: %s" destination))
      (copy-file file destination nil))))

(defun org-ocr-import--load-bundle (bundle-directory)
  "Validate BUNDLE-DIRECTORY and return a plist of import data."
  (let* ((bundle-directory
          (file-name-as-directory (file-truename bundle-directory)))
         (manifest (org-ocr-import--read-manifest bundle-directory))
         (source-hash
          (org-ocr-import--required-string "source_sha256" manifest))
         (source-relative
          (org-ocr-import--required-string "source_path" manifest))
         (source-file
          (org-ocr-import--bundle-file bundle-directory source-relative))
         (note-relative
          (org-ocr-import--required-string "note_path" manifest))
         (note-file
          (org-ocr-import--bundle-file bundle-directory note-relative))
         (assets (org-ocr-import--collect-assets bundle-directory manifest))
         (suggested-title
          (or (org-ocr-import--get "suggested_title" manifest)
              (file-name-base
               (org-ocr-import--required-string "source_filename" manifest))))
         (markdown
          (with-temp-buffer
            (insert-file-contents note-file)
            (buffer-string))))
    (org-ocr-import--verify-hash source-file source-hash "original source")
    (setq markdown
          (org-ocr-import--markdown-attachment-links markdown assets))
    (list :bundle-directory bundle-directory
          :manifest manifest
          :source-hash source-hash
          :source-file source-file
          :assets assets
          :suggested-title suggested-title
          :markdown markdown)))

(defun org-ocr-import--prepare-import (bundle-directory title root-level)
  "Load BUNDLE-DIRECTORY and prepare Org content for ROOT-LEVEL."
  (let* ((data (org-ocr-import--load-bundle bundle-directory))
         (suggested-title (plist-get data :suggested-title))
         (title (or title suggested-title))
         (assets (plist-get data :assets))
         ;; Markdown level 1 should become a child of the Transcription heading,
         ;; which is one level below the OCR root.
         (org-body
          (org-ocr-import--pandoc
           (plist-get data :markdown) (1+ root-level))))
    (unless (and (stringp title)
                 (not (string-empty-p (string-trim title))))
      (user-error "The node title cannot be empty"))
    (org-ocr-import--verify-asset-links org-body assets)
    (plist-put data :title title)
    (plist-put data :root-level root-level)
    (plist-put data :org-body org-body)
    data))

(defun org-ocr-import--maybe-sync-and-check-duplicate (source-hash)
  "Synchronize org-roam if requested and reject duplicate SOURCE-HASH."
  (when org-ocr-import-sync-before-import
    (org-roam-db-sync))
  (when-let ((existing (org-ocr-import--existing-node source-hash)))
    (user-error "OCR source is already imported as %s"
                (org-roam-node-title existing))))

(defun org-ocr-import--create-node-file (data)
  "Create a new Org-roam file from prepared import DATA."
  (let* ((manifest (plist-get data :manifest))
         (title (plist-get data :title))
         (source-hash (plist-get data :source-hash))
         (source-file (plist-get data :source-file))
         (assets (plist-get data :assets))
         (org-body (plist-get data :org-body))
         (created-at (org-ocr-import--required-string "created_at" manifest))
         (destination
          (org-ocr-import--destination title created-at source-hash))
         (id (org-id-new))
         (attachment-files (cons source-file (mapcar #'cadr assets)))
         (basenames (mapcar #'file-name-nondirectory attachment-files))
         (filetags (org-ocr-import--filetags-line))
         (subtree
          (org-ocr-import--subtree-text
           title 1 id manifest (file-name-nondirectory source-file) org-body))
         (file-text
          (concat (format "#+title: %s\n" (org-ocr-import--one-line title))
                  (or filetags "")
                  "\n"
                  subtree))
         (buffer nil)
         (attachment-directory nil)
         (attachment-directory-created nil)
         (file-created nil)
         (success nil))
    (unless (= (length basenames)
               (length (delete-dups (copy-sequence basenames))))
      (user-error "The original and an asset have the same basename"))
    (unwind-protect
        (progn
          (with-temp-file destination
            (insert file-text))
          (setq file-created t)
          (setq buffer (find-file-noselect destination))
          (with-current-buffer buffer
            (org-mode)
            (goto-char (point-min))
            (unless (re-search-forward org-heading-regexp nil t)
              (error "Generated Org file has no node heading"))
            (goto-char (line-beginning-position))
            (let ((root-marker (copy-marker (point)))
                  subtree-end)
              (org-end-of-subtree t t)
              (setq subtree-end (copy-marker (point) t))
              (goto-char root-marker)
              (when (org-attach-dir nil)
                (error "New node ID unexpectedly has an attachment directory"))
              (setq attachment-directory (org-attach-dir t))
              (setq attachment-directory-created t)
              (org-ocr-import--copy-attachments
               attachment-files attachment-directory)
              (goto-char root-marker)
              (org-attach-sync)
              (org-ocr-import--set-local-attachment-directories
               root-marker subtree-end attachment-directory)
              (set-marker root-marker nil)
              (set-marker subtree-end nil))
            (save-buffer))
          (setq success t)
          (condition-case err
              (org-id-add-location id destination)
            (error
             (message "OCR import: could not update org-id locations: %s"
                      (error-message-string err))))
          (condition-case err
              (org-roam-db-update-file destination)
            (error
             (message "OCR import: node created; run org-roam-db-sync (%s)"
                      (error-message-string err))))
          destination)
      (unless success
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (when (and attachment-directory-created
                   attachment-directory
                   (file-directory-p attachment-directory))
          (delete-directory attachment-directory t))
        (when (and file-created (file-exists-p destination))
          (delete-file destination))))))

(defun org-ocr-import--roam-file-p (file)
  "Return non-nil if FILE is inside `org-roam-directory'."
  (and file
       (boundp 'org-roam-directory)
       org-roam-directory
       (file-in-directory-p
        (file-truename file)
        (file-name-as-directory (file-truename org-roam-directory)))))

(defun org-ocr-import--capture-here-target ()
  "Capture the current Org buffer and parent heading for asynchronous import."
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not an Org buffer"))
  (unless buffer-file-name
    (user-error "Current Org buffer is not visiting a file"))
  (when (and org-ocr-import-here-require-roam
             (not (org-ocr-import--roam-file-p buffer-file-name)))
    (user-error "Current file is not inside org-roam-directory"))
  (let ((parent-marker nil))
    (unless (org-before-first-heading-p)
      (save-excursion
        (org-back-to-heading t)
        (setq parent-marker (copy-marker (point)))))
    (list :buffer (current-buffer)
          :parent-marker parent-marker)))

(defun org-ocr-import--here-root-level (target)
  "Return the heading level to use for TARGET."
  (let ((parent (plist-get target :parent-marker)))
    (if (and parent (marker-position parent))
        (with-current-buffer (marker-buffer parent)
          (save-excursion
            (goto-char parent)
            (1+ (org-outline-level))))
      1)))

(defun org-ocr-import--insert-into-target (data target)
  "Insert prepared import DATA into captured TARGET and return buffer file name."
  (let* ((buffer (plist-get target :buffer))
         (parent-marker (plist-get target :parent-marker))
         (manifest (plist-get data :manifest))
         (title (plist-get data :title))
         (level (plist-get data :root-level))
         (source-file (plist-get data :source-file))
         (assets (plist-get data :assets))
         (org-body (plist-get data :org-body))
         (id (org-id-new))
         (attachment-files (cons source-file (mapcar #'cadr assets)))
         (basenames (mapcar #'file-name-nondirectory attachment-files))
         (subtree
          (org-ocr-import--subtree-text
           title level id manifest (file-name-nondirectory source-file) org-body
           (org-ocr-import--heading-tags))))
    (unless (buffer-live-p buffer)
      (user-error "The target Org buffer no longer exists"))
    (unless (= (length basenames)
               (length (delete-dups (copy-sequence basenames))))
      (user-error "The original and an asset have the same basename"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (user-error "Target buffer is no longer in Org mode"))
      (let ((change-group (prepare-change-group))
            (root-marker nil)
            (subtree-end nil)
            (attachment-directory nil)
            (attachment-directory-created nil)
            (success nil))
        (unwind-protect
            (progn
              (activate-change-group change-group)
              (save-excursion
                (if (and parent-marker (marker-position parent-marker))
                    (progn
                      (goto-char parent-marker)
                      (org-end-of-subtree t t)
                      (unless (bolp) (insert "\n")))
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n")))
                (setq root-marker (copy-marker (point)))
                (insert subtree)
                (setq subtree-end (copy-marker (point) t))
                (goto-char root-marker)
                (unless (looking-at-p org-heading-regexp)
                  (error "Inserted OCR subtree does not begin with a heading"))
                (setq attachment-directory (org-attach-dir t))
                (setq attachment-directory-created t)
                (org-ocr-import--copy-attachments
                 attachment-files attachment-directory)
                (goto-char root-marker)
                (org-attach-sync)
                (org-ocr-import--set-local-attachment-directories
                 root-marker subtree-end attachment-directory))
              (save-buffer)
              (accept-change-group change-group)
              (setq success t)
              (condition-case err
                  (org-id-add-location id buffer-file-name)
                (error
                 (message "OCR import: could not update org-id locations: %s"
                          (error-message-string err))))
              (when (org-ocr-import--roam-file-p buffer-file-name)
                (condition-case err
                    (org-roam-db-update-file buffer-file-name)
                  (error
                   (message "OCR import: run org-roam-db-sync (%s)"
                            (error-message-string err)))))
              (goto-char root-marker)
              (org-show-entry)
              (org-show-subtree)
              buffer-file-name)
          (unless success
            (ignore-errors (cancel-change-group change-group))
            (when (and attachment-directory-created
                       attachment-directory
                       (file-directory-p attachment-directory))
              (delete-directory attachment-directory t)))
          (when root-marker (set-marker root-marker nil))
          (when subtree-end (set-marker subtree-end nil)))))))

(defun org-ocr-import--api-key ()
  "Retrieve the Mistral API key through `auth-source'."
  (let* ((auth-sources (or org-ocr-import-auth-sources auth-sources))
         (query (append
                 (list :host org-ocr-import-auth-host)
                 (when org-ocr-import-auth-user
                   (list :user org-ocr-import-auth-user))
                 (list :require '(:secret) :max 1)))
         (entry (car (apply #'auth-source-search query)))
         (secret (and entry (auth-info-password entry))))
    (unless (and (stringp secret) (not (string-empty-p secret)))
      (user-error "No Mistral API key found in auth-source for host %S%s"
                  org-ocr-import-auth-host
                  (if org-ocr-import-auth-user
                      (format " and user %S" org-ocr-import-auth-user)
                    "")))
    secret))

(defun org-ocr-import--ensure-program (program description)
  "Return executable PROGRAM or signal a user error mentioning DESCRIPTION."
  (or (executable-find program)
      (user-error "%s executable not found: %s" description program)))

(defun org-ocr-import--ensure-python-script ()
  "Return the configured OCR Python script path or signal a user error."
  (unless (and org-ocr-import-python-script
               (file-regular-p org-ocr-import-python-script))
    (user-error "Set `org-ocr-import-python-script' to org-ocr-import.py"))
  (expand-file-name org-ocr-import-python-script))

(defun org-ocr-import--process-log-buffer ()
  "Return the shared OCR process log buffer."
  (get-buffer-create "*org-ocr-import*"))

(defun org-ocr-import--process-failure (what process stderr-buffer)
  "Report failed PROCESS performing WHAT and show STDERR-BUFFER."
  (let ((status (process-exit-status process)))
    (with-current-buffer stderr-buffer
      (goto-char (point-max))
      (insert (format "\n%s failed (exit %s).\n" what status)))
    (display-buffer stderr-buffer)
    (message "%s failed; see %s" what (buffer-name stderr-buffer))))

(defun org-ocr-import--start-rasterize (input output callback &optional error-callback)
  "Rasterize INPUT to OUTPUT asynchronously, then call CALLBACK with OUTPUT.
If the process fails and ERROR-CALLBACK is non-nil, call it after reporting the
failure."
  (let* ((program
          (org-ocr-import--ensure-program
           org-ocr-import-ghostscript-program "Ghostscript"))
         (input (expand-file-name input))
         (output (expand-file-name output))
         (factor org-ocr-import-raster-downscale-factor)
         (render-dpi (* org-ocr-import-raster-dpi factor))
         (stderr-buffer (org-ocr-import--process-log-buffer)))
    (unless (and (file-regular-p input)
                 (string-equal (downcase (or (file-name-extension input) ""))
                               "pdf"))
      (user-error "Rasterization input must be a PDF: %s" input))
    (unless (> factor 0)
      (user-error "org-ocr-import-raster-downscale-factor must be positive"))
    (make-directory (file-name-directory output) t)
    (with-current-buffer stderr-buffer
      (goto-char (point-max))
      (insert (format "\nRasterizing %s -> %s\n" input output)))
    (message "OCR: rasterizing %s..." (file-name-nondirectory input))
    (make-process
     :name "org-ocr-import-rasterize"
     :buffer nil
     :stderr stderr-buffer
     :noquery t
     :command
     (list program "-q" "-dNOPAUSE" "-dBATCH"
           "-sDEVICE=pdfimage24"
           (format "-r%d" render-dpi)
           (format "-dDownScaleFactor=%d" factor)
           "-sCompression=Flate"
           "-o" output input)
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (if (and (= (process-exit-status process) 0)
                  (file-regular-p output))
             (progn
               (message "OCR: rasterized %s" (file-name-nondirectory output))
               (funcall callback output))
           (org-ocr-import--process-failure
            "Rasterization" process stderr-buffer)
           (when error-callback
             (funcall error-callback))))))))

(defun org-ocr-import--parse-ocr-result (stdout-buffer)
  "Parse the Python OCR result from STDOUT-BUFFER."
  (with-current-buffer stdout-buffer
    (goto-char (point-min))
    (json-parse-buffer
     :object-type 'alist
     :array-type 'list
     :null-object nil
     :false-object nil)))

(defun org-ocr-import--start-ocr
    (input output callback &optional force error-callback)
  "Run OCR asynchronously on INPUT.
If OUTPUT is non-nil, pass it as the bundle directory.  On success call
CALLBACK with the parsed JSON result.  With FORCE, pass --force.  If the
process fails or its result cannot be parsed, call ERROR-CALLBACK when non-nil."
  (let* ((uv (org-ocr-import--ensure-program org-ocr-import-uv-program "uv"))
         (script (org-ocr-import--ensure-python-script))
         (input (expand-file-name input))
         (output (and output (expand-file-name output)))
         (api-key (org-ocr-import--api-key))
         (stdout-buffer (generate-new-buffer " *org-ocr-import-stdout*"))
         (stderr-buffer (org-ocr-import--process-log-buffer))
         (command
          (append (list uv "run" "--script" script)
                  (when output (list "--output" output))
                  (when force (list "--force"))
                  (list input))))
    (unless (file-regular-p input)
      (user-error "OCR input does not exist: %s" input))
    (with-current-buffer stderr-buffer
      (goto-char (point-max))
      (insert (format "\nOCR input: %s\n" input)))
    (message "OCR: sending %s to Mistral..." (file-name-nondirectory input))
    ;; Bind process-environment locally so MISTRAL_API_KEY is inherited only by
    ;; this subprocess and is not installed globally in Emacs.
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "MISTRAL_API_KEY" api-key)
      (make-process
       :name "org-ocr-import-mistral"
       :buffer stdout-buffer
       :stderr stderr-buffer
       :noquery t
       :command command
       :sentinel
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (if (= (process-exit-status process) 0)
               (let (result parse-error)
                 (condition-case err
                     (setq result (org-ocr-import--parse-ocr-result stdout-buffer))
                   (error (setq parse-error err)))
                 (if parse-error
                     (progn
                       (display-buffer stdout-buffer)
                       (message
                        "OCR succeeded but its JSON result could not be parsed: %s"
                        (error-message-string parse-error))
                       (when error-callback
                         (funcall error-callback)))
                   (kill-buffer stdout-buffer)
                   (message "OCR: bundle ready at %s"
                            (org-ocr-import--get "bundle" result))
                   (funcall callback result)))
             (when (buffer-live-p stdout-buffer)
               (kill-buffer stdout-buffer))
             (org-ocr-import--process-failure "Mistral OCR" process stderr-buffer)
             (when error-callback
               (funcall error-callback)))))))))

(defun org-ocr-import--default-raster-output (input)
  "Return the default persistent rasterized output path for INPUT."
  (concat (file-name-sans-extension (expand-file-name input)) ".raster.pdf"))

(defun org-ocr-import--default-bundle-output (input)
  "Return the default OCR bundle path associated with INPUT."
  (concat (file-name-sans-extension (expand-file-name input)) ".ocr"))

(defun org-ocr-import--read-bundle-directory ()
  "Prompt for an OCR bundle directory."
  (file-name-as-directory
   (read-directory-name "OCR bundle directory: " nil nil t)))

;;;###autoload
(defun org-ocr-import-rasterize (input output)
  "Rasterize INPUT PDF at `org-ocr-import-raster-dpi' into OUTPUT."
  (interactive
   (let* ((input (read-file-name "PDF to rasterize: " nil nil t nil
                                 (lambda (f) (or (file-directory-p f)
                                                 (string-match-p "\\.pdf\\'" f)))))
          (default (org-ocr-import--default-raster-output input))
          (output (read-file-name "Rasterized PDF: "
                                  (file-name-directory default)
                                  default nil
                                  (file-name-nondirectory default))))
     (list input output)))
  (org-ocr-import--start-rasterize
   input output
   (lambda (path)
     (message "Rasterized PDF written to %s" path))))

;;;###autoload
(defun org-ocr-import-run (input &optional force)
  "Run only the Mistral OCR step on INPUT.
The Python script chooses its normal bundle directory beside INPUT.
With prefix argument FORCE, replace an existing recognized bundle."
  (interactive (list (read-file-name "OCR input PDF/image: " nil nil t)
                     current-prefix-arg))
  (org-ocr-import--start-ocr
   input nil
   (lambda (result)
     (message "OCR bundle: %s" (org-ocr-import--get "bundle" result)))
   force))

;;;###autoload
(defun org-ocr-import-new (bundle-directory &optional title)
  "Import BUNDLE-DIRECTORY as a new Org-roam file and return its path."
  (interactive (list (org-ocr-import--read-bundle-directory)))
  (let* ((initial (org-ocr-import--load-bundle bundle-directory))
         (suggested (plist-get initial :suggested-title))
         (title (or title
                    (if (called-interactively-p 'interactive)
                        (read-string "Node title: " suggested)
                      suggested)))
         (data (org-ocr-import--prepare-import bundle-directory title 1)))
    (org-ocr-import--maybe-sync-and-check-duplicate
     (plist-get data :source-hash))
    (let ((destination (org-ocr-import--create-node-file data)))
      (when (called-interactively-p 'interactive)
        (find-file destination))
      (message "Imported OCR bundle as %s" destination)
      destination)))

;;;###autoload
(defun org-ocr-import-here (bundle-directory &optional title target)
  "Import BUNDLE-DIRECTORY as a new OCR heading in an existing Org file.
When point is inside a heading, insert the OCR note as a child of that heading.
Before the first heading, append a new top-level heading.  TARGET is an
internal captured target used by asynchronous workflows."
  (interactive (list (org-ocr-import--read-bundle-directory)))
  (let* ((target (or target (org-ocr-import--capture-here-target)))
         (root-level (org-ocr-import--here-root-level target))
         (initial (org-ocr-import--load-bundle bundle-directory))
         (suggested (plist-get initial :suggested-title))
         (title (or title
                    (if (called-interactively-p 'interactive)
                        (read-string "Heading title: " suggested)
                      suggested)))
         (data (org-ocr-import--prepare-import
                bundle-directory title root-level)))
    (org-ocr-import--maybe-sync-and-check-duplicate
     (plist-get data :source-hash))
    (let ((file (org-ocr-import--insert-into-target data target)))
      (message "Imported OCR bundle into %s" file)
      file)))

(defun org-ocr-import--suggested-title-from-file (file)
  "Return a human-readable title suggested by FILE's basename."
  (let ((stem (file-name-base file)))
    (setq stem
          (replace-regexp-in-string
           "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[-_ ]+" "" stem))
    (setq stem (replace-regexp-in-string "[-_]+" " " stem))
    (setq stem (string-trim (replace-regexp-in-string "[[:space:]]+" " " stem)))
    (if (string-empty-p stem) "Handwritten note" stem)))

(defun org-ocr-import--process
    (source import-kind target title force)
  "Rasterize SOURCE, OCR it, then import according to IMPORT-KIND.
IMPORT-KIND is either `new' or `here'.  TARGET is used for `here'.  TITLE is
the already chosen Org node/heading title."
  (let* ((source (expand-file-name source))
         (temp-directory (make-temp-file "org-ocr-import-" t))
         ;; Give the rasterized file the original basename so the OCR manifest
         ;; records a useful source filename while still hashing/storing the
         ;; rasterized OCR input rather than the large vector export.
         (raster-output
          (expand-file-name (file-name-nondirectory source) temp-directory))
         (bundle-output (org-ocr-import--default-bundle-output source)))
    (cl-labels
        ((cleanup ()
           (when-let ((parent (and target (plist-get target :parent-marker))))
             (set-marker parent nil))
           (when (file-directory-p temp-directory)
             (delete-directory temp-directory t)))
         (abort ()
           (cleanup))
         (finish-import (result)
           (condition-case err
               (let ((bundle (org-ocr-import--get "bundle" result)))
                 (unless (and (stringp bundle) (file-directory-p bundle))
                   (error "OCR result did not contain a valid bundle path"))
                 (pcase import-kind
                   ('new (org-ocr-import-new bundle title))
                   ('here (org-ocr-import-here bundle title target))
                   (_ (error "Unknown import kind: %S" import-kind))))
             (error
              (message "OCR completed, but import failed: %s"
                       (error-message-string err)))
             (quit
              (message "OCR completed; import was cancelled")))
           (cleanup))
         (after-raster (raster)
           (condition-case err
               (org-ocr-import--start-ocr
                raster bundle-output #'finish-import force #'abort)
             (error
              (cleanup)
              (signal (car err) (cdr err))))))
      (condition-case err
          (org-ocr-import--start-rasterize
           source raster-output #'after-raster #'abort)
        (error
         (cleanup)
         (signal (car err) (cdr err)))))))

;;;###autoload
(defun org-ocr-import-process-new (source title &optional force)
  "Rasterize SOURCE, run OCR, and import it as a new Org-roam file.
TITLE is chosen before the asynchronous pipeline starts.  With prefix argument
FORCE, pass --force to the OCR script."
  (interactive
   (let* ((source (read-file-name "Source XNotes PDF: " nil nil t))
          (title (read-string "Node title: "
                              (org-ocr-import--suggested-title-from-file source))))
     (list source title current-prefix-arg)))
  (org-ocr-import--process source 'new nil title force))

;;;###autoload
(defun org-ocr-import-process-here (source title &optional force)
  "Rasterize SOURCE, run OCR, and insert it into the current Org file.
If point is inside a heading, the OCR import becomes a child of that heading.
The target buffer/heading and TITLE are captured before asynchronous work begins.
With prefix argument FORCE, pass --force to the OCR script."
  (interactive
   (let* ((source (read-file-name "Source XNotes PDF: " nil nil t))
          (title (read-string "Heading title: "
                              (org-ocr-import--suggested-title-from-file source))))
     (list source title current-prefix-arg)))
  (let ((target (org-ocr-import--capture-here-target)))
    (org-ocr-import--process source 'here target title force)))

(provide 'org-ocr-import)

;;; org-ocr-import.el ends here
