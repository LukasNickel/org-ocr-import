# org-ocr-import

A small package for importing handwritten notes into my emacs org-roam system.
This is heavily assisted by ChatGPT and tailored to my own needs, so I do not plan to publish this on MELPA anytime soon.

It consists of three main parts:
1. Run OCR on PDF. This is based on the mistral-ocr model. You need an api key for that in your secrets storage
2. Convert generated markdown to org mode using pandoc and org-attach to embed images
3. Import that as a new org-roam node


[showcase.webm](https://github.com/user-attachments/assets/6f4876c9-e6bb-4aca-941f-e056e2beb656)


Notes:
- I realized webm does not work as input, so needed to print to pdf
- My pdf tools in emacs are broken right now, oops
- My org roam prints a few "processing files" messages. This is because emacs was just started, it has nothing to do with the ocr stuff
- I did not show the directory afterwards. The mistral ocr bundle is also saved alongside the pdf:

<img width="1066" height="726" alt="result" src="https://github.com/user-attachments/assets/5ee02990-43ac-4ec8-bbcf-41860788694a" />


I am still debating whether I want to keep that or delete it afterwards/do it in a temp dir.
