#!/usr/bin/env python3
"""Apply the two-number entry routes to the installed managed dialplan."""
from __future__ import annotations

import re
import sys
from pathlib import Path

ENTRY = "[phone-assistant-inbound]"
NEXT = "[phone-assistant-record-call]"


def replace_once(source: str, old: str, new: str) -> str | None:
    match source.count(old):
        case 1:
            return source.replace(old, new, 1)
        case _:
            return None


def render(base: str, entry: str, shared: str, dedicated: str, owner: str) -> str | None:
    if any(re.fullmatch(r"\+44[1-9][0-9]{9}", number) is None for number in (shared, dedicated)):
        return None
    if shared == dedicated or re.fullmatch(r"[a-z0-9-]{1,40}", owner) is None:
        return None
    before, marker, tail = base.partition(ENTRY)
    _, next_context, rest = tail.partition(NEXT)
    if not marker or not next_context:
        return None
    routes = entry.replace("@@PRO_DIGITS@@", dedicated[1:])
    routes = routes.replace("@@SHARED_DIGITS@@", shared[1:]).replace("@@PRO_ID@@", owner)
    changes = (
        (' same => n,Set(CDR(tf_did)=${GLOBAL(PA_AA_DID)})', f' same => n,Set(CDR(tf_did)={dedicated})'),
        (' same => n(enabled),GotoIf($["${GLOBAL(PA_INCOMING_SCREENING_ENABLED)}"="1"]?screen:rejected)',
         f' same => n(enabled),ExecIf($["${{CHANNEL(endpoint)}}"="phone-assistant-telematica"]?Set(PA_ACCOUNT={owner}))\n'
         ' same => n,GotoIf($["${GLOBAL(PA_INCOMING_SCREENING_ENABLED)}"="1"]?screen:rejected)'),
        (' same => n(assistant),Set(PA_CALL_ID=', f' same => n(assistant),Set(PA_ACCOUNT={owner})\n same => n,Set(PA_CALL_ID='),
        (' same => n(announce),Set(PA_OWNER_AUTHORIZED=0)', ' same => n(announce),Set(PA_ACCOUNT=${MASTER_CHANNEL(PA_ACCOUNT)})\n'
         ' same => n,Set(PA_OWNER_AUTHORIZED=0)'),
    )
    result = before + routes + "\n" + next_context + rest
    for old, new in changes:
        updated = replace_once(result, old, new)
        if updated is None:
            return None
        result = updated
    return result


def main(args: list[str]) -> int:
    match args:
        case [base, entry, shared, dedicated, owner, destination]:
            try:
                source = Path(base).read_text()
                template = Path(entry).read_text()
                result = render(source, template, shared, dedicated, owner)
                if result is None:
                    print("Account routing does not match the managed dialplan.", file=sys.stderr)
                    return 1
                Path(destination).write_text(result)
                return 0
            except OSError:
                print("Could not read or write the managed dialplan.", file=sys.stderr)
                return 1
        case _:
            print("Usage: render-asterisk-accounts.py BASE TEMPLATE SHARED_DID PRO_DID ACCOUNT OUTPUT", file=sys.stderr)
            return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
