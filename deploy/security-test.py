#!/usr/bin/env python3
"""Functional and security checks for a running pdfServer.

Run ON the server, against the live instance or a test one:

    scp deploy/security-test.py root@147.182.143.224:/root/
    ssh root@147.182.143.224 'python3 /root/security-test.py 3000 /root/pdfServer'

PORT is the pdfServer port, WORKDIR its working directory. The attack checks
use harmless markers (touching files in /var/lib/pdfserver), so a failure
is visible but does no damage. It leaves test documents in WORKDIR's save/,
outbox/ and inbox/ (names starting test-, read-, ssrf, inject-, ...), and
the TikZ check uploads one small circle to Cloudflare Images.
Written for the 2026-11-01 hardening (commit 7cf920e): 26 checks.
"""
import json, os, subprocess, sys, urllib.request, urllib.error

PORT, WORK = sys.argv[1], sys.argv[2]
BASE = f"http://127.0.0.1:{PORT}"
MARK = "/var/lib/pdfserver"          # writable by the service: markers would land here
results = []

def post(path, body, timeout=120):
    req = urllib.request.Request(BASE + path, data=json.dumps(body).encode(),
                                 headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return r.status, r.read().decode(errors="replace")
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode(errors="replace")

def get(path):
    try:
        with urllib.request.urlopen(BASE + path, timeout=30) as r:
            return r.status, r.read()
    except urllib.error.HTTPError as e:
        return e.code, b""

def pdf_text(name):
    path = os.path.join(WORK, "outbox", name)
    if not os.path.exists(path):
        return None
    return subprocess.run(["pdftotext", path, "-"], capture_output=True, text=True).stdout

def check(label, ok, detail=""):
    results.append(ok)
    print(("PASS " if ok else "FAIL ") + label + (f"  ({detail})" if detail else ""))

def doc(id_, content, urls=(), packages=()):
    return {"id": id_, "content": content,
            "urlList": [{"url": u, "filename": f} for u, f in urls],
            "packageList": list(packages)}

ART = r"\documentclass{article}" "\n" r"\begin{document}" "\n%s\n" r"\end{document}" "\n"
GFX = (r"\documentclass{article}" "\n" r"\usepackage{graphicx}" "\n"
       r"\graphicspath{{%s/image/}}" "\n" r"\begin{document}" "\n%s\n" r"\end{document}" "\n")

for f in ("pwned1", "pwned2", "pwned3", "pwned4"):
    try: os.remove(os.path.join(MARK, f))
    except FileNotFoundError: pass

print("== still works")
s, b = post("/pdf", doc("test-math.tex", ART % r"Math: $\int_0^1 x^2\,dx = \frac13$."))
t = pdf_text("test-math.pdf")
check("simple document typesets", s == 200 and t and "Math" in t, f"{s} {b[:80]}")

img = "https://www.w3.org/Graphics/PNG/nurbcup2si.png"
s, b = post("/pdf", doc("test-image.tex", GFX % (WORK, r"\includegraphics[width=3cm]{%s}" % img),
                        urls=[(img, "cup.png")]))
check("image via urlList downloads and typesets", s == 200 and '"hasErrors":false' in b, b[:120])
check("  image file was fetched", os.path.exists(os.path.join(WORK, "image", "cup.png"))
      or os.path.exists(os.path.join(WORK, "save", "test-image.tex")))

s, b = post("/pdf", doc("Sketch 1 schrödinger.tex", ART % "Odd name."))
check("odd document name is cleaned, not refused", s == 200 and "Sketch-1-schr-dinger.pdf" in b, b[:120])

s, b = get("/pdf/test-math.pdf")
check("GET /pdf/<name> serves the PDF", s == 200 and b[:5] == b"%PDF-", str(s))

print("== attacks fail")
s, b = post("/pdf", doc(f"x; touch {MARK}/pwned1; .tex", ART % "hi"))
check("shell injection in the id does nothing", not os.path.exists(f"{MARK}/pwned1"), b[:80])

s, b = post("/pdf", doc(f"../../../..{MARK}/escape.tex", ART % "escape"))
check("path traversal in the id stays in inbox/outbox", not os.path.exists(f"{MARK}/escape.tex")
      and not os.path.exists(f"{MARK}/escape.pdf"), b[:80])

s, b = post("/pdf", doc("read-passwd.tex", ART % r"X\input{/etc/passwd}Y"))
t = pdf_text("read-passwd.pdf") or ""
check(r"\input{/etc/passwd} is not read", "root:x:0" not in t and "daemon" not in t, b[:80])

s, b = post("/pdf", doc("read-env.tex", ART % (r"X\input{%s/.env}Y" % WORK)))
t = pdf_text("read-env.pdf") or ""
check(r"\input{.env} is not read", "CF_API_KEY" not in t and "CF_ACCOUNT" not in t, b[:80])

s, b = post("/pdf", doc("read-other.tex", ART % r"X\input{/root/greppit/README.md}Y"))
t = pdf_text("read-other.pdf") or ""
readme = open("/root/greppit/README.md").read().splitlines()[2][:40]
check("files outside pdfServer's folder are not read", readme not in t, b[:80])

s, b = post("/pdf", doc("write18.tex", ART % (r"\immediate\write18{touch %s/pwned2}hi" % MARK)))
check(r"\write18 runs nothing", not os.path.exists(f"{MARK}/pwned2"), b[:80])

for u in ["file:///etc/passwd", "http://169.254.169.254/metadata/v1/", "http://127.0.0.1:3010/hello",
          "http://localhost:5432/", "gopher://example.com/", "http://10.0.0.1/x.png"]:
    s, b = post("/pdf", doc("ssrf.tex", GFX % (WORK, r"\includegraphics{%s}" % u), urls=[(u, "ssrf.png")]))
    check(f"refuses to fetch {u}", not os.path.exists(os.path.join(WORK, "image", "ssrf.png")))

evil = 'http://example.com/a"$(touch %s/pwned3)".png' % MARK
s, b = post("/pdf", doc("inject-url.tex", GFX % (WORK, r"\includegraphics{%s}" % evil)))
check("shell injection via an image URL in the LaTeX does nothing", not os.path.exists(f"{MARK}/pwned3"))

s, b = post("/pdf", doc("inject-fname.tex", ART % "hi",
                        urls=[("https://www.w3.org/Graphics/PNG/nurbcup2si.png", f"x; touch {MARK}/pwned4")]))
check("shell injection via an image file name does nothing", not os.path.exists(f"{MARK}/pwned4"))

s, b = post("/pdf", doc("pkg.tex", ART % "hi", packages=["../../../etc/passwd"]))
check("package name can't copy files from outside package/",
      not os.path.exists(os.path.join(WORK, "inbox", "passwd")), b[:60])

for p in ["/pdf/..%2F.env", "/pdf/..%2F..%2Fetc%2Fpasswd", "/tar/..%2F.env", "/pdf/.env"]:
    s, b = get(p)
    check(f"GET {p} refused", s != 200 or b"CF_" not in b, str(s))

print("== tikz2png")
s, b = post("/tikz2png", {"name": "test-circle.json", "content": r"\begin{tikzpicture}\draw (0,0) circle (1cm);\end{tikzpicture}"})
check("TikZ renders and uploads", s == 200 and "imagedelivery.net" in b, b[:120])
s, b = post("/tikz2png", {"name": "../../x", "content": r"\begin{tikzpicture}\node{\input{/etc/passwd}};\end{tikzpicture}"})
check(r"TikZ \input{/etc/passwd} is refused", s != 200 or "imagedelivery" not in b, b[:100])

print(f"\n{sum(results)}/{len(results)} passed")
