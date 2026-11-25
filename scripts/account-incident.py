#!/usr/bin/env python3
"""Print categories from the recent Asterisk routing trace before rollback."""
from __future__ import annotations

import json
import re
from collections import Counter
from pathlib import Path

WORK = Path("/home/stefan/.cache/baerbel-hs/deploy")
FIELDS = re.compile(r'"(PA_REQUEST_USER|PA_TO_USER|PA_ENTRY|PA_ACCOUNT)=([^"]*)"')
CONTEXT = re.compile(r"@(baerbel-account-entry|phone-assistant-(?:inbound|resume|screening)):[0-9]+")
FAILURES = {
    "Failed to authenticate": "sip_authentication_failed",
    "No matching endpoint": "sip_endpoint_unmatched",
    "FastAGI failed": "fastagi_failed",
}


def number(value: str) -> str:
    match value:
        case _ if value.startswith("00"):
            return "+" + value[2:]
        case _ if value.startswith("0"):
            return "+44" + value[1:]
        case _ if value.startswith("44"):
            return "+" + value
        case _:
            return value


def categories(value: str, shared: str, dedicated: str, account: str) -> str:
    match number(value):
        case "" | "(null)":
            return "empty"
        case digits if digits == shared or digits == "shared":
            return "shared"
        case digits if digits == dedicated or digits == account:
            return "bristol"
        case _:
            return "other"


def projection(log: str, shared: str, dedicated: str, account: str) -> dict[str, int]:
    result: Counter[str] = Counter()
    for line in log.splitlines():
        result.update(CONTEXT.findall(line))
        for marker, code in FAILURES.items():
            if marker in line:
                result[code] += 1
        for field, value in FIELDS.findall(line):
            category = categories(value, shared, dedicated, account)
            result[f"{field}:{category}"] += 1
    return dict(result)


def main() -> None:
    shared, dedicated, account = (WORK / "account-routes").read_text().split()
    log = Path("/var/log/asterisk/messages.log")
    with log.open("rb") as stream:
        end = stream.seek(0, 2)
        stream.seek(max(0, end - 4_000_000))
        tail = stream.read().decode(errors="replace")
    summary = projection(tail, shared, dedicated, account)
    # The report contains only fixed codes and counts.
    result = json.dumps(summary, sort_keys=True)
    Path("/run/baerbel-account-incident.json").write_text(result + "\n")
    print("Asterisk routing trace:", result)


if __name__ == "__main__":
    main()
