#!/usr/bin/env python3
"""Send 6 exports to pdfServer at the same moment and check each PDF has its
own text and its image. Run ON the server:

    python3 concurrency-test.py PORT WORKDIR      # e.g. 3000 /root/pdfServer

Before the compile lock (2026-11-01), 3 of 6 came back broken: each job's
`rm -f inbox/*.tex image/*` deleted the others' files mid-compile. Leaves
conc-* files in WORKDIR's save/, outbox/ and inbox/.
"""
import json, os, subprocess, sys, threading, urllib.request, uuid

port, work = sys.argv[1], sys.argv[2]
IMG = "https://www.w3.org/Graphics/PNG/nurbcup2si.png"
run = uuid.uuid4().hex[:8]          # new content each run, so the cache never hits
results = {}

def job(i):
    marker = f"MARKER{i}x{run}"
    tex = (r"\documentclass{article}" "\n" r"\usepackage{graphicx}" "\n"
           r"\graphicspath{{" + work + r"/image/}}" "\n" r"\begin{document}" "\n"
           + marker + "\n\n" r"\includegraphics[width=2cm]{" + IMG + "}\n\n"
           + "Filler text. " * 400 + "\n" r"\end{document}" "\n")
    body = {"id": f"conc-{i}.tex", "content": tex,
            "urlList": [{"url": IMG, "filename": f"conc-img-{i}.png"}], "packageList": []}
    req = urllib.request.Request(f"http://127.0.0.1:{port}/pdf", data=json.dumps(body).encode(),
                                 headers={"Content-Type": "application/json"})
    try:
        out = json.loads(urllib.request.urlopen(req, timeout=300).read())
    except Exception as e:
        results[i] = f"request failed: {e}"
        return
    pdf = os.path.join(work, "outbox", f"conc-{i}.pdf")
    if os.path.exists(pdf):
        text = subprocess.run(["pdftotext", pdf, "-"], capture_output=True, text=True).stdout
        images = max(0, subprocess.run(["pdfimages", "-list", pdf], capture_output=True, text=True).stdout.count("\n") - 2)
    else:
        text, images = "", 0
    ok = out.get("hasErrors") is False and marker in text and images >= 1
    results[i] = "ok" if ok else (f"BROKEN (hasErrors={out.get('hasErrors')}, "
                                  f"own text={marker in text}, images={images})")

threads = [threading.Thread(target=job, args=(i,)) for i in range(6)]
for t in threads: t.start()
for t in threads: t.join()
for i in range(6):
    print(f"  export {i}: {results[i]}")
print(f"  {sum(r == 'ok' for r in results.values())}/6 correct")
