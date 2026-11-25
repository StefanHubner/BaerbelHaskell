#!/usr/bin/env python3
"""Prepare a separate authenticated A&A endpoint for the dedicated DID."""
from __future__ import annotations

import re
import sys
from pathlib import Path

SECTION = re.compile(r"^\[([^]]+)\][^\n]*\n", re.MULTILINE)
GLOBAL = re.compile(r"^[ \t]*type[ \t]*=[ \t]*global\b", re.MULTILINE)
ORDER = re.compile(r"^(\s*endpoint_identifier_order\s*=)[ \t]*([^;\n]+)", re.MULTILINE)
DEFAULT = "ip,username,anonymous"
PRIVATE_INCLUDE = '#include "phone-assistant-bristol-pjsip.conf"'


def endpoint(text: str, name: str) -> str | None:
    sections = list(SECTION.finditer(text))
    for current, following in zip(sections, sections[1:] + [None]):
        if current[1] == name:
            return text[current.end():following.start() if following else len(text)].strip()
    return None


def identifier_order(text: str) -> str | None:
    if GLOBAL.search(text) is None:
        return text.rstrip() + "\n\n[baerbel-identification]\ntype=global\nendpoint_identifier_order=header," + DEFAULT + "\n"
    sections = list(SECTION.finditer(text))
    for current, following in zip(sections, sections[1:] + [None]):
        start = current.end()
        end = following.start() if following else len(text)
        block = text[start:end]
        if GLOBAL.search(block) is None:
            continue
        setting = ORDER.search(block)
        order = setting[2].strip() if setting else DEFAULT
        ordered = ",".join(["header"] + [item.strip() for item in order.split(",") if item.strip() != "header"])
        updated = ORDER.sub(lambda match: match[1] + ordered, block, count=1) if setting else block.rstrip() + "\nendpoint_identifier_order=" + ordered + "\n\n"
        return text[:start] + updated + text[end:]
    return None


def render(parent: str, trunk: str, dedicated: str, username: str, password: str) -> tuple[str, str] | None:
    existing = endpoint(trunk, "phone-assistant-aa")
    identify = endpoint(trunk, "phone-assistant-aa-identify")
    if not existing or not identify:
        return None
    carrier = re.search(r"^match=([0-9.,/]+)$", identify, re.MULTILINE)
    if carrier is None or not re.fullmatch(r"\+44[1-9][0-9]{9}", dedicated):
        return None
    if not re.fullmatch(r"[A-Za-z0-9_-]{1,64}", username) or not re.fullmatch(r"[A-Za-z0-9_.+-]{12,128}", password):
        return None
    clone = re.sub(r"^auth=.*$", "auth=phone-assistant-aa-bristol-auth", existing, flags=re.MULTILINE)
    clone = re.sub(r"^identify_by=.*$", "identify_by=header", clone, flags=re.MULTILINE)
    if clone == existing or "identify_by=header" not in clone or "auth=phone-assistant-aa-bristol-auth" not in clone:
        return None
    permit = "\n".join("permit=" + network for network in carrier[1].split(","))
    national = dedicated[3:]
    match = f"/sips?:(0|0044|[+]?44){national}@/"
    include = (
        f"[phone-assistant-aa-bristol]\n{clone}\ndeny=0.0.0.0/0\ndeny=::/0\n{permit}\n\n"
        f"[phone-assistant-aa-bristol-auth]\ntype=auth\nauth_type=userpass\nusername={username}\npassword={password}\n\n"
        "[phone-assistant-aa-bristol-identify]\ntype=identify\nendpoint=phone-assistant-aa-bristol\n"
        f"match_header=To: {match}\n"
    )
    updated = identifier_order(parent)
    if updated is None:
        return None
    updated = updated if PRIVATE_INCLUDE in updated else updated.rstrip() + "\n" + PRIVATE_INCLUDE + "\n"
    return updated, include


def main(args: list[str]) -> int:
    match args:
        case [parent, trunk, did, contact, secret, destination]:
            folder = Path(destination)
            try:
                result = render(Path(parent).read_text(), Path(trunk).read_text(), did, contact, Path(secret).read_text().strip())
                if result is None:
                    print("Cannot prepare the dedicated SIP endpoint from the installed trunk.", file=sys.stderr)
                    return 1
                config, include = result
                (folder / "pjsip.conf").write_text(config)
                (folder / "phone-assistant-bristol-pjsip.conf").write_text(include)
                return 0
            except OSError:
                print("Could not read or write the SIP configuration.", file=sys.stderr)
                return 1
        case _:
            return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
