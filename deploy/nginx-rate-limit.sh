#!/usr/bin/env bash
# Rate-limit pdfserv.app in nginx: 30 requests a minute per client, after
# a burst of 30; over that, 429. pdfServer compiles LaTeX and uploads to
# Cloudflare Images for anyone, so this slows abuse. Real use is far below
# the limit: the busiest client made about 100 requests in 10 days.
# Run from your Mac, in the pdfServer2 repo:
#
#   ssh root@147.182.143.224 'bash -s' < deploy/nginx-rate-limit.sh
#
# Edits the installed site in place (keeping certbot's HTTPS lines), backs
# it up to /root/nginx-backups/, and restores the backup if `nginx -t`
# fails. Running it twice is safe.
set -euo pipefail

F=/etc/nginx/sites-available/pdfserver
B=/root/nginx-backups/pdfserver.$(date +%Y-%m-%d-%H%M%S)-pre-ratelimit

if grep -q pdfserv_limit "$F"; then
    echo "Already rate-limited; nothing to do."
    exit 0
fi

mkdir -p /root/nginx-backups
cp -p "$F" "$B"
echo "Backup: $B"

python3 - "$F" <<'PY'
import sys
p = sys.argv[1]
t = open(p, encoding="utf-8").read()
zone = """# pdfserv.app is not behind Cloudflare's proxy, so $binary_remote_addr is
# the real client.
limit_req_zone $binary_remote_addr zone=pdfserv_limit:10m rate=30r/m;

"""
anchor = "server {\n    server_name pdfserv.app;\n"
assert t.count(anchor) == 1, "HTTPS server block not found as expected"
t = t.replace(anchor, zone + anchor
              + "\n    limit_req zone=pdfserv_limit burst=30 nodelay;\n    limit_req_status 429;\n", 1)
open(p, "w", encoding="utf-8").write(t)
PY

if nginx -t; then
    systemctl reload nginx
    echo "OK: nginx reloaded with the pdfserv.app rate limit."
    diff -u "$B" "$F" || true
else
    cp -p "$B" "$F"
    echo "nginx -t failed: backup restored, nginx not reloaded."
    exit 1
fi
