#!/usr/bin/env python3

import sys, os, re, tempfile, subprocess
from email import message_from_bytes, policy
from urllib.parse import quote

msg = message_from_bytes(sys.stdin.buffer.read(), policy=policy.default)
d = tempfile.mkdtemp(prefix="mail-")

html = None
cids = {}
for i, part in enumerate(msg.walk()):
    if part.is_multipart():
        continue
    cid = (part.get("Content-ID") or "").strip("<> \n")
    if part.get_content_type() == "text/html" and html is None and not part.get_filename():
        html = part.get_content()
    elif cid:
        ext = part.get_content_subtype()
        path = os.path.join(d, f"{i}.{ext}")
        with open(path, "wb") as f:
            f.write(part.get_payload(decode=True))
        cids[cid] = path

if html is None:
    sys.exit("no text/html part found")

for cid, path in cids.items():
    uri = "file://" + quote(path)
    for variant in (cid, quote(cid, safe="")):
        html = html.replace("cid:" + variant, uri)

out = os.path.join(d, "mail.html")
with open(out, "w", encoding="utf-8") as f:
    f.write(html)
subprocess.Popen(["xdg-open", out], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
