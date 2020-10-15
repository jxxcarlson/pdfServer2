# pdfServer

A start on server to produce PDF files from LaTeX files.  

- The server accepts POST requests 
at localhost:3000/pdf with JSON data like that below, stores the content
in a file, e.g., `A2.tex`, generates a PDF file, e.g., `A2.pdf`, and, if successful,
returns the id of the document as a string, which in this case is "A2".

- Upon receipt of the reply from the POST request, the client can make a 
  GET request `/pdf/:id` to retrieve the PDF files.  In the case at hand, 
  one makes the GET request `/pdf/A2`.

{"id": "A2", "title": "Test", "content": "\\documentclass{article}\n\\begin{document}\n$a^2 + b^2 = c^2$\n\\end{document}\n"}


