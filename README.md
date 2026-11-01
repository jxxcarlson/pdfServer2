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

pdfServer runs at **https://pdfserv.app**, on the DigitalOcean droplet
**rose2** (`root@147.182.143.224`, Ubuntu 24.04). It moved there from
**rose** on 2026-11-01.

**DNS.** pdfserv.app's DNS is at Squarespace Domains (formerly Google
Domains), not Cloudflare. Its A record points straight at rose2: it isn't
proxied, and has a one-hour TTL.

**Layout on rose2:**

- `/root/pdfServer` is the working directory:
  - the program `pdfServer`;
  - `tikz2png/`, `package/`, and `.env` (`CF_ACCOUNT_ID`, `CF_API_KEY`
    for Cloudflare Images; root-only, mode 600);
  - the folders `inbox/`, `outbox/`, `image/`, `save/`, `cf-image/`
    and `pdfcache/`, owned by `pdfserver`.
- `/root/pdfServer/pdfServer2` is this repo, on `master`.
- `/etc/systemd/system/pdfServer.service` runs it as the unprivileged
  system user **`pdfserver`**, sandboxed:
  - `ProtectSystem=strict`, `ProtectHome=read-only`, `NoNewPrivileges`,
    `PrivateTmp`, no capabilities;
  - it can write only to the folders above and `/var/lib/pdfserver`,
    its home, where TeX keeps font caches.
- `/etc/nginx/sites-available/pdfserver` passes `https://pdfserv.app` to
  port 3000, with a Let's Encrypt certificate (certbot renews it). It
  allows 30 requests a minute per client, after a burst of 30, then 429
  (`deploy/nginx-rate-limit.sh`).

**Never start it by hand or `pkill` it.** Use `systemctl` (status,
restart) or `deploy.sh`. A copy started by hand runs as root, outside
the sandbox, and keeps holding port 3000, which stops the real service
from starting. That's how a stray backend ran unnoticed on rose for
months.

**To update the server**, commit and push to `master`, then run from
your Mac:

    ssh root@147.182.143.224 '. /root/.ghcup/env && cd /root/pdfServer/pdfServer2 && bash ./deploy.sh'

`deploy.sh`:

1. pulls `master` (fast-forward only);
2. regenerates the `.cabal` file and builds with Stack. The old server
   keeps serving meanwhile, and a failed build stops here;
3. stops the service, backs up the old program as `pdfServer.prev`, and
   installs the new program and `tikz2png/tikz2png.sh`;
4. starts the service and checks it. A healthy run ends with `active`,
   `HTTP 404` (`/` has no page) and `Deployed: <commit>`.

Check afterwards:

    systemctl is-active pdfServer          # on rose2: "active"
    curl https://pdfserv.app/hello         # from anywhere
    journalctl -u pdfServer -n 50          # on rose2: recent log

**Tests after a change**, run on rose2 against the live server. They
leave a few test files behind, named in each script's header:

    scp deploy/security-test.py deploy/concurrency-test.py root@147.182.143.224:/root/
    ssh root@147.182.143.224 'python3 /root/security-test.py 3000 /root/pdfServer'
    ssh root@147.182.143.224 'python3 /root/concurrency-test.py 3000 /root/pdfServer'

- `security-test.py` (26 checks): documents, images and TikZ still work,
  and shell injection, path traversal, TeX file reads, `\write18` and
  fetches of internal or `file://` URLs all fail.
- `concurrency-test.py`: 6 simultaneous exports each come back intact.
- Set `PDFSERVER_PORT` to run a second copy on another port for testing,
  in its own working directory.

**Security, in short.** pdfServer compiles LaTeX sent by anyone, so:

- every name from a request is reduced to `[A-Za-z0-9._-]`
  (`src/Safe.hs`);
- images are fetched only from public `http(s)` addresses, by `curl`
  without a shell;
- at startup the server sets TeX's paranoid mode (`openin_any=p`,
  `openout_any=p`, `shell_escape=f`), and every TeX run has
  `-no-shell-escape`;
- one LaTeX job runs at a time, because jobs share `inbox/`, `image/` and
  `outbox/`;
- `outbox/` keeps PDFs 1 day, and `save/` keeps copies of inputs 7 days.

Background is in the greppit repo, `docs/changelog.md`, 2026-11-01.

**Build setup.** Stack resolver `lts-22.44` (GHC 9.6.7), installed with
ghcup on rose2. `/root/.stack/config.yaml` has `system-ghc: true`, so
Stack uses ghcup's GHC.

## Concordance tool (standalone)

`Concordance.hs`, at the top of the repo, is a small command-line program
for trying out the module `src/Concordance.hs` by hand. That module is
what the server uses (`src/Pdf.hs`) to point its error reports at the
right place in the Scripta source.

**What it does.** Scripta's LaTeX export puts a comment line
`%%% Line N` before each block, recording which line of the Scripta
source the block came from. LaTeX reports errors by LaTeX line number
(`l.NNN` in its log). For each error line in the log, the program finds:

- `latexSrc`: the LaTeX line of the error (from `l.NNN`);
- `begin`, `end`: the paragraph of LaTeX around it, bounded by blank
  lines or `%%% Line` comments;
- `scriptaSrc`: the Scripta source line, from the nearest `%%% Line N`
  above the error.

It writes these as CSV to `concordance.txt`.

**Input files.** The names are fixed, and the files are read from the
current folder:

- `latex-error-report.txt`: the LaTeX log, or any text containing its
  `l.NNN` lines;
- `entropy.tex`: the LaTeX source, with its `%%% Line N` comments.

Neither file is in the repo. Use any exported document and its log, for
example from `save/` and `outbox/` on the server, renamed to these names.

**Build.** It needs only GHC; the module uses just `base` and
`containers`. From the repo root:

    ghc -i -isrc -outputdir .concordance-build Concordance.hs -o Concordance

The `-i` before `-isrc` matters. It removes the current folder from the
import path, so that `import Concordance` finds `src/Concordance.hs`
rather than this program's own `Concordance.hs`. Plain `ghc -isrc
Concordance.hs` fails with "File name does not match module name". The
build folder, the `Concordance` program and `concordance.txt` are all in
`.gitignore`.

**Run.**

    ./Concordance

**Example.** With this `entropy.tex`:

    \documentclass{article}
    \begin{document}
    %%% Line 1
    A first paragraph.

    %%% Line 4
    Some text with $\frac{1}{2$ a broken formula
    that continues here.

    %%% Line 9
    Another \undefinedmacro here.
    \end{document}

and a `latex-error-report.txt` containing `l.7 Some text with ...` and
`l.11 Another \undefinedmacro`, it prints `Created concordance with 2
entries`, and `concordance.txt` is:

    latexSrc, begin, end, scriptaSrc
    7, 6, 8, 4
    11, 10, 12, 9

So the error on LaTeX line 7 is in the paragraph on lines 6–8 and came
from Scripta line 4; the one on line 11 came from Scripta line 9.

## Haskell notes

- `stack install MissingH` is needed
- `hoogle`