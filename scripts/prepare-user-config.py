#!/usr/bin/env python3
"""Convert the existing dotenv configuration into a private Haskell user file."""
from __future__ import annotations

import json
import os
import sys
import tempfile
from collections.abc import Mapping
from pathlib import Path
from typing import TypedDict, cast

from dotenv import dotenv_values


class ConfigurationRules(TypedDict):
    aliases: dict[str, list[str]]


class Rules(TypedDict):
    configuration: ConfigurationRules


class GlobalConfig(TypedDict):
    rules: Rules


def alias_value(values: Mapping[str, str | None], name: str, aliases: list[str]) -> str:
    names = [name] + aliases
    candidates = map(values.get, names)
    return next(filter(None, candidates), "")


def resolve_aliases(values: Mapping[str, str | None], aliases: Mapping[str, list[str]]) -> dict[str, str | None]:
    resolved: dict[str, str | None] = {name: alias_value(values, name, fallbacks) for name, fallbacks in aliases.items()}
    return dict(values) | resolved


def configuration(values: Mapping[str, str | None], aliases: Mapping[str, list[str]]) -> bytes:
    resolved = resolve_aliases(values, aliases)
    environment = {name: value for name, value in resolved.items() if value is not None}
    document = {"environment": environment}
    content = json.dumps(document, ensure_ascii=False, indent=2) + "\n"
    return content.encode("utf-8")


def read_environment(source: Path) -> dict[str, str | None]:
    with source.open(encoding="utf-8") as stream:
        return dotenv_values(stream=stream, interpolate=False)


def read_aliases(source: Path) -> dict[str, list[str]]:
    with source.open(encoding="utf-8") as stream:
        config = cast(GlobalConfig, json.load(stream))
        return config["rules"]["configuration"]["aliases"]


def write_private(destination: Path, content: bytes) -> None:
    destination.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=".baerbel-user-", dir=destination.parent)
    try:
        with os.fdopen(descriptor, "wb") as stream:
            os.fchmod(stream.fileno(), 0o600)
            stream.write(content)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, destination)
    finally:
        Path(temporary).unlink(missing_ok=True)


def main(arguments: list[str]) -> int:
    source, destination, global_config = map(Path, arguments)
    try:
        values = read_environment(source)
        aliases = read_aliases(global_config)
        content = configuration(values, aliases)
        write_private(destination, content)
    except OSError:
        print("Unable to prepare private user configuration.", file=sys.stderr)
        return 1
    return 0


match __name__:
    case "__main__":
        arguments = sys.argv[1:]
        result = main(arguments)
        sys.exit(result)
