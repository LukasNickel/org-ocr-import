# org-ocr-import

A small package for importing handwritten notes into my emacs org-roam system.
This is heavily assisted by ChatGPT and tailored to my own needs, so I do not plan to publish this on MELPA anytime soon.

It consists of three main parts:
1. Run OCR on PDF. This is based on the mistral-ocr model. You need an api key for that in your secrets storage
2. Convert generated markdown to org mode using pandoc and org-attach to embed images
3. Import that as a new org-roam node


