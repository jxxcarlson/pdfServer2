# PdfServer

pdfServer runs  as a print server for LaTeX files.
It is written in 165 lines of Haskell code using
the Scotty web server library.

## Description

The server accepts POST requests at /pdf/ with payload 
a record containing a document id, a title, and the 
contents of a LaTeX file.

Upon receipt of such a request, the server runs
xelatex on the LaTeX file, after which
GET requests to /pdf/:id will return 
a link the corresponding PDF file.

These days the id is a normalized version of the 
document title.  Thus "Introduction to Quantum Mechanics"
will have "introduction-to-quantum-mechanics.tex" 
as id.


```
{"id": "test-document.tex", "title": "Test Document", "content": "\\documentclass{article}\n\\begin{document}\n$a^2 + b^2 = c^2$\n\\end{document}\n"}
```

## Tar Archives

POST requests to /tar/ with the same payload as to /pdf/
will generate a tar archive with contents the LaTeX document
and a subfolder "image" with a copy of each of the
image files found in the document. After processing,
GET requests to /tar/:id will return a copy of the 
tar archive.  

## Local operation

- `stack run`

Test if the server is up with `http://localhost:3000/hello`

Do not use `https` on localhost!!


## Operation of the server

The server is installed as `http://pdfServ.app` at
my DigitalOcean "rose" box in `./pdfServer`

**To update the server:**

- `git pull` if need be
- `stack build`
- `stack ghc app/Main.hs, mv app/Main pdfServer`
- `reboot`

## Haskell notes
 
- `stack install MissingH` is needed
- `hoogle`


This is a test.


## Commands

- netstat: list open ports

```
root@rose:~/pdfServer# netstat -lntu
Active Internet connections (only servers)
Proto Recv-Q Send-Q Local Address           Foreign Address         State
tcp        0      0 0.0.0.0:80              0.0.0.0:*               LISTEN
tcp        0      0 127.0.0.53:53           0.0.0.0:*               LISTEN
tcp        0      0 0.0.0.0:22              0.0.0.0:*               LISTEN
tcp        0      0 127.0.0.1:5432          0.0.0.0:*               LISTEN
tcp        0      0 0.0.0.0:3000            0.0.0.0:*               LISTEN
tcp        0      0 0.0.0.0:443             0.0.0.0:*               LISTEN
tcp6       0      0 :::22                   :::*                    LISTEN
udp        0      0 127.0.0.53:53           0.0.0.0:*
```

https://www.digitalocean.com/community/tutorials/opening-a-port-on-linux

  526  snap connect nmap:network-control
  527  nmap localhost -p 3001
  528  sudo ufw status | grep 300
