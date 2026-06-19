#!/usr/bin/env bash
#
# Deploy pdfServer2 to production (the "rose" droplet).
#
# Run ON the server, from this checkout:
#     cd /root/pdfServer/pdfServer2 && ./deploy.sh
#
# Layout on rose:
#   /root/pdfServer             <- systemd WorkingDirectory; holds the live binary
#                                  (pdfServer) and the runtime scripts the binary calls
#   /root/pdfServer/pdfServer2  <- this git checkout (the build source)
#
# The service is managed by systemd (pdfServer.service, Restart=always). That matters:
#   * We must restart via systemctl, not `pkill ... &` — otherwise systemd respawns the
#     old binary within RestartSec while a second copy is launched by hand (port clash).
#   * We must STOP the service before copying the binary, or the copy hits ETXTBSY
#     ("Text file busy") because Linux refuses to overwrite a running executable.
#   * The binary resolves tikz2png.sh relative to its WorkingDirectory (/root/pdfServer),
#     NOT this repo — so the runtime copy must be synced from the repo on every deploy.
#
# Safe by construction: the build runs while the old server is still serving, so a build
# failure aborts (set -e) before any downtime. The previous binary/script are backed up
# to *.prev for a one-line rollback (see bottom of this file).
set -euo pipefail

REPO=/root/pdfServer/pdfServer2
LIVE=/root/pdfServer
SERVICE=pdfServer.service

cd "$REPO"

echo "==> Updating source (fast-forward only)"
git pull --ff-only

echo "==> Building (old server still serving; a failure here aborts before any downtime)"
stack build
stack install --local-bin-path .

echo "==> Stopping $SERVICE"
systemctl stop "$SERVICE"

echo "==> Installing new binary + runtime tikz2png.sh (backing up previous to *.prev)"
cp -p "$LIVE/pdfServer"                 "$LIVE/pdfServer.prev"
cp -p "$LIVE/tikz2png/tikz2png.sh"      "$LIVE/tikz2png/tikz2png.sh.prev"
cp    "$REPO/pdfServerScotty-exe"       "$LIVE/pdfServer"
cp    "$REPO/tikz2png/tikz2png.sh"      "$LIVE/tikz2png/tikz2png.sh"

echo "==> Starting $SERVICE"
systemctl start "$SERVICE"

echo "==> Health check"
sleep 2
systemctl is-active "$SERVICE"
curl -s -m 5 -o /dev/null -w "pdfServer :3000 -> HTTP %{http_code}\n" http://localhost:3000/ || true

echo "==> Deployed: $(git -C "$REPO" log --oneline -1)"

# Rollback (if the new build misbehaves):
#   systemctl stop pdfServer.service
#   cp /root/pdfServer/pdfServer.prev            /root/pdfServer/pdfServer
#   cp /root/pdfServer/tikz2png/tikz2png.sh.prev /root/pdfServer/tikz2png/tikz2png.sh
#   systemctl start pdfServer.service
