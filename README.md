# pdfServer

A start on server to produce PDF files from LaTeX files.  The server accepts POST requests 
at localhost:300/pdf with JSON data like that below, stores the content
if a file, e.g., `A2.tex`, generates a PDF file, e.g., `A2.pdf`, and returns a link to the PDF file.

{"id": "A2", "title": "Test", "content": "\\documentclass{article}\n\\begin{document}\n$a^2 + b^2 = c^2$\n\\end{document}\n"}


