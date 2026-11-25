#!/usr/bin/env python3
"""Convert reviewed libphonenumber XML into the separate Haskell phone reference data."""
from __future__ import annotations

import hashlib
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Generic, Iterable, TypeAlias, TypeVar, TypedDict
from xml.etree import ElementTree as XML

RESTRICTED: tuple[str, ...] = ("premiumRate", "tollFree", "sharedCost", "voip", "personalNumber", "pager", "uan", "voicemail")
ORDINARY: tuple[str, ...] = ("fixedLine", "mobile")
T = TypeVar("T")


@dataclass(frozen=True)
class Parsed(Generic[T]):
    value: T


@dataclass(frozen=True)
class Problem:
    message: str


Result: TypeAlias = Parsed[T] | Problem
Pattern: TypeAlias = tuple[str, dict[int, int]]


class Description(TypedDict):
    pattern: str
    lengths: list[int]


class Prefix(TypedDict):
    pattern: str
    replacement: tuple[int, str] | None


class Region(TypedDict):
    leading: str | None
    general: Description
    restricted: list[Description]
    ordinary: list[Description]
    prefix: Prefix | None


class Metadata(TypedDict):
    version: str
    source: str
    sha256: str
    countries: dict[str, list[Region]]


def collect(values: Iterable[Result[T]]) -> Result[list[T]]:
    result: list[T] = []
    for value in values:
        match value:
            case Problem() as problem:
                return problem
            case Parsed(item):
                result.append(item)
    return Parsed(result)


def ere(value: str, complete: bool) -> Result[Pattern]:
    source = re.sub(r"\s", "", value)
    match re.search(r"\(\?[^:]|\\[^d]", source):
        case None:
            return Parsed(convert_pattern(source, complete))
        case _:
            return Problem(f"unsupported phone metadata pattern: {source}")


def convert_pattern(source: str, complete: bool) -> Pattern:
    groups: dict[int, int] = {}
    captures = filter(lambda item: item[1].group(1) is None, enumerate(re.finditer(r"\((\?:)?", source)))
    for original, (converted, _) in enumerate(captures, start=1):
        groups[original] = converted + 2
    pattern = source.replace("(?:", "(").replace(r"\d", "[0-9]")
    ending = {True: "$", False: ""}[complete]
    return f"^({pattern}){ending}", groups


def lengths(value: str) -> list[int]:
    result: list[int] = []
    for part in value.split(","):
        match part[:1]:
            case "[":
                first, last = map(int, part[1:-1].split("-"))
                result.extend(range(first, last + 1))
            case _:
                result.append(int(part))
    return result


def description(element: XML.Element) -> Result[Description]:
    possible = element.find("possibleLengths")
    match possible:
        case None:
            return Problem(f"missing phone metadata lengths: {element.tag}")
    match ere(element.findtext("nationalNumberPattern", ""), True):
        case Problem() as problem:
            return problem
        case Parsed(pair):
            pattern, _ = pair
            return Parsed({"pattern": pattern, "lengths": lengths(possible.attrib["national"])})


def optional_pattern(value: str | None) -> Result[str | None]:
    match value:
        case None:
            return Parsed(None)
    match ere(value, False):
        case Problem() as problem:
            return problem
        case Parsed(pair):
            pattern, _ = pair
            return Parsed(pattern)


def prefix_rule(source: str | None, transform: str) -> Result[Prefix | None]:
    match source:
        case None:
            return Parsed(None)
        case value:
            return prefix_pattern(ere(value, False), transform)


def prefix_pattern(parsed: Result[Pattern], transform: str) -> Result[Prefix | None]:
    match parsed:
        case Problem() as problem:
            return problem
        case Parsed(pair):
            pattern, groups = pair
            return transform_prefix(pattern, groups, transform)


def transform_prefix(pattern: str, groups: dict[int, int], transform: str) -> Result[Prefix | None]:
    match transform:
        case "":
            return Parsed({"pattern": pattern, "replacement": None})
        case _:
            return transformed_prefix(pattern, groups, transform)


def transformed_prefix(pattern: str, groups: dict[int, int], transform: str) -> Result[Prefix | None]:
    match re.fullmatch(r"([0-9]*)\$([0-9])", transform):
        case None:
            return Problem(f"unsupported phone metadata transform: {transform}")
        case matched:
            index = int(matched[2])
            return Parsed({"pattern": pattern, "replacement": (groups[index], matched[1])})


def region(element: XML.Element) -> Result[Region]:
    children = list(filter(lambda child: child.tag in RESTRICTED + ORDINARY, element))
    match collect(map(description, children)):
        case Problem() as problem:
            return problem
        case Parsed(descriptions):
            types = dict(zip((child.tag for child in children), descriptions))
            return region_patterns(element, types)


def region_patterns(element: XML.Element, types: dict[str, Description]) -> Result[Region]:
    general = ere(element.findtext("generalDesc/nationalNumberPattern", ""), True)
    leading = optional_pattern(element.get("leadingDigits"))
    prefix = prefix_rule(element.get("nationalPrefixForParsing", element.get("nationalPrefix")), element.get("nationalPrefixTransformRule", ""))
    match general:
        case Problem() as problem:
            return problem
        case Parsed(pair):
            pattern, _ = pair
            return complete_region(pattern, types, leading, prefix)


def complete_region(pattern: str, types: dict[str, Description], leading: Result[str | None], prefix: Result[Prefix | None]) -> Result[Region]:
    match leading:
        case Problem() as problem:
            return problem
        case Parsed(initial):
            return region_value(pattern, types, initial, prefix)


def region_value(pattern: str, types: dict[str, Description], leading: str | None, prefix: Result[Prefix | None]) -> Result[Region]:
    match prefix:
        case Problem() as problem:
            return problem
        case Parsed(replacement):
            possible = sorted({number for entry in types.values() for number in entry["lengths"]})
            restricted = [types[name] for name in filter(types.__contains__, RESTRICTED)]
            ordinary = [types[name] for name in filter(types.__contains__, ORDINARY)]
            return Parsed({"leading": leading, "general": {"pattern": pattern, "lengths": possible}, "restricted": restricted, "ordinary": ordinary, "prefix": replacement})


def metadata(version: str, raw: bytes, root: XML.Element) -> Result[Metadata]:
    territories = root.findall("./territories/territory")
    territories.sort(key=lambda item: item.get("mainCountryForCode") != "true")
    match collect(map(region, territories)):
        case Problem() as problem:
            return problem
        case Parsed(regions):
            countries: dict[str, list[Region]] = {}
            for element, entry in zip(territories, regions):
                countries.setdefault(element.attrib["countryCode"], []).append(entry)
            source = f"https://raw.githubusercontent.com/google/libphonenumber/v{version}/resources/PhoneNumberMetadata.xml"
            return Parsed({"version": version, "source": source, "sha256": hashlib.sha256(raw).hexdigest(), "countries": countries})


def update(version: str, source: str, target: str) -> Result[None]:
    try:
        raw = Path(source).read_bytes()
        root = XML.fromstring(raw)
        parsed = metadata(version, raw, root)
        return save(target, parsed)
    except (OSError, XML.ParseError, json.JSONDecodeError, KeyError, ValueError) as problem:
        return Problem(str(problem))


def save(target: str, parsed: Result[Metadata]) -> Result[None]:
    match parsed:
        case Problem() as problem:
            return problem
        case Parsed(value):
            path = Path(target)
            output = json.dumps({"metadata": value}, indent=2, ensure_ascii=False) + "\n"
            path.write_text(output)
            return Parsed(None)


def main(arguments: list[str]) -> None:
    match arguments:
        case [version, source, target]:
            result = update(version, source, target)
            report(result)
        case _:
            report(Problem("usage: update-phone-metadata.py VERSION XML PHONE_JSON"))


def report(result: Result[None]) -> None:
    match result:
        case Parsed(None):
            pass
        case Problem(message):
            print(message, file=sys.stderr)
            sys.exit(1)


match __name__:
    case "__main__":
        main(sys.argv[1:])
