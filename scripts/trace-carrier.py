#!/usr/bin/env python3
"""Capture carrier SIP routing metadata without keeping SIP messages."""
from __future__ import annotations

import json
import re
import subprocess
from collections import Counter
from pathlib import Path


WORK = Path("/home/stefan/.cache/baerbel-hs/deploy")
FILTER = "udp port 5060 and (net 81.187.30.110/31 or net 81.187.30.112/29 or net 90.155.3.0/24 or net 90.155.103.0/24)"
PACKET = re.compile(r"(?m)^\d\d:\d\d:\d\d\.\d+\s+")
INVITE = re.compile(r"\bINVITE sips?:([^\s@]+)@[^\s]+ SIP/2\.0")
TO = re.compile(r"(?im)^(?:To|t):\s*[^\r\n]*?sips?:([^\s@>]+)@")
RESPONSE = re.compile(r"\bSIP/2\.0 ([1-6][0-9][0-9])\s")
INVITE_SEQ = re.compile(r"(?im)^CSeq:\s*\d+\s+INVITE\b")


def category(value: str, shared: str, dedicated: str) -> str:
    for name, number in (("old", shared), ("bristol", dedicated)):
        if value in {number, number[1:], "00" + number[1:], "0" + number[3:]}:
            return name
    return "other" if value else "missing"


def routing(report: str, shared: str, dedicated: str) -> dict[str, int]:
    result: Counter[str] = Counter()
    for packet in PACKET.split(report):
        invite = INVITE.search(packet)
        match invite:
            case re.Match():
                target = TO.search(packet)
                to_user = target.group(1) if target else ""
                request = category(invite.group(1), shared, dedicated)
                called = category(to_user, shared, dedicated)
                auth = bool(re.search(r"(?im)^Authorization:", packet))
                result[f"invite:request={request},to={called},digest={auth}"] += 1
            case _:
                reply = RESPONSE.search(packet)
                if reply and INVITE_SEQ.search(packet):
                    result["reply:" + reply.group(1)] += 1
    return dict(result)


def main() -> None:
    shared, dedicated, _ = (WORK / "account-routes").read_text().split()
    print("Listening for A&A signalling for 30 seconds. Call the Bristol number now.", flush=True)
    run = subprocess.run(["timeout", "30", "/usr/bin/tcpdump", "-i", "any", "-nn", "-l", "-s", "0", "-A", FILTER],
                         capture_output=True, check=False)
    result = routing(run.stdout.decode(errors="replace"), shared, dedicated)
    summary = {"capture_started": run.returncode in (0, 124), "routing": result}
    redacted = json.dumps(summary, sort_keys=True)
    Path("/run/baerbel-carrier-trace.json").write_text(redacted + "\n")
    print(redacted)


if __name__ == "__main__":
    main()
