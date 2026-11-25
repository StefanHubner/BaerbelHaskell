#!/usr/bin/env python3
"""Move the live account JSON and its existing state into the user database."""
from __future__ import annotations

import json
import os
import sqlite3
import subprocess
import sys
from contextlib import closing
from pathlib import Path
from typing import TypedDict, cast


class Account(TypedDict):
    plan: str
    name: str
    phone: str
    environment: dict[str, str]


class Legacy(TypedDict):
    environment: dict[str, str]
    accounts: dict[str, Account]


PIN = "PHONE_ASSISTANT_PRIVATE_PIN_HASH"
SHARED_USER_SETTINGS = {
    "PHONE_ASSISTANT_GREETING", "PHONE_ASSISTANT_OWNER_NOTES", "PHONE_ASSISTANT_WINDGURU",
}
PLAN_SETTINGS = {
    "Standard": {"PHONE_ASSISTANT_DIRECT_CALLS"},
    "Pro": {
        "HOME_PHONE_ENABLED", "TELEMATICA_OUTBOUND_ENABLED", "INCOMING_MESSAGES_ENABLED",
        "INCOMING_SMS_ENABLED", "INCOMING_EVENTS_FILE",
    },
}
RECEIPT_SOURCES = {
    "gmail": "PHONE_ASSISTANT_GMAIL_STATE_FILE",
    "calendar": "PHONE_ASSISTANT_CALENDAR_STATE_FILE",
}


def legacy_rows(path: str, select: str) -> list[tuple[object, ...]]:
    source = Path(path).expanduser()
    if not path or not source.is_file():
        return []
    with closing(sqlite3.connect(source.as_uri() + "?mode=ro", uri=True)) as db:
        return db.execute(select).fetchall()


def copy_receipts(db: sqlite3.Connection, user: str, account: Account, values: dict[str, str]) -> None:
    db.execute("DELETE FROM delivery_receipts WHERE user_id=?", (user,))
    for service, setting in RECEIPT_SOURCES.items():
        credential = f"PHONE_ASSISTANT_{service.upper()}_TOKEN_FILE"
        if not account["environment"].get(credential):
            continue
        records = legacy_rows(values[setting], "SELECT request_id, fingerprint, result, created_at FROM sends")
        db.executemany("INSERT INTO delivery_receipts VALUES (?, ?, ?, ?, ?, ?)",
                       [(user, service) + row for row in records])
    if account["plan"] != "Pro" or values["INCOMING_SMS_ENABLED"] not in ("true", "1"):
        return
    sources = (
        ("PHONE_ASSISTANT_SMS_STATE_FILE", "sms_receipts",
         "SELECT request_id, fingerprint, status, created_at, kind, fallback FROM sms_sends"),
        ("INCOMING_EVENTS_STATE_FILE", "event_cursors",
         "SELECT file_key, position, anchor_length, anchor FROM incoming_event_cursors"),
    )
    for setting, table, select in sources:
        records = legacy_rows(values[setting], select)
        db.execute(f"DELETE FROM {table} WHERE user_id=?", (user,))
        placeholders = {"sms_receipts": "?, ?, ?, ?, ?, ?, ?", "event_cursors": "?, ?, ?, ?, ?"}
        db.executemany(f"INSERT INTO {table} VALUES ({placeholders[table]})", [(user,) + row for row in records])


def copy_account(db: sqlite3.Connection, user: str, account: Account, defaults: dict[str, str]) -> None:
    values = account["environment"]
    resolved = defaults | values
    path = Path(resolved["PHONE_ASSISTANT_PROFILE_FILE"]).expanduser()
    memory: dict[str, object] = json.loads(path.read_text()) if path.is_file() else {}
    updated = memory.get("updated_at")
    entries = cast(dict[str, str], memory.get("values", {}))
    db.execute(
        "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT(id) DO UPDATE SET "
        "user_type=excluded.user_type, name=excluded.name, phone=excluded.phone, "
        "pin_hash=excluded.pin_hash, memory_updated_at=excluded.memory_updated_at",
        (user, account["plan"], account["name"], account["phone"], values[PIN], updated),
    )
    keep = SHARED_USER_SETTINGS | PLAN_SETTINGS[account["plan"]]
    settings = [(user, key, value) for key, value in values.items()
                if key in keep and value != defaults.get(key, "").replace("{{owner_name}}", account["name"])]
    db.execute("DELETE FROM user_settings WHERE user_id=?", (user,))
    db.executemany("INSERT INTO user_settings VALUES (?, ?, ?)", settings)
    db.execute("DELETE FROM user_memory WHERE user_id=?", (user,))
    db.executemany("INSERT INTO user_memory VALUES (?, ?, ?)", [(user, key, value) for key, value in entries.items()])
    copy_receipts(db, user, account, resolved)


def migrate(global_path: Path, legacy_path: Path, config_path: Path, database: Path, create: bool) -> int:
    legacy = cast(Legacy, json.loads(legacy_path.read_text()))
    defaults: dict[str, str] = json.loads(global_path.read_text())["defaults"]
    if create and not database.exists():
        fd = os.open(database, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        os.close(fd)
        with closing(sqlite3.connect(database)) as db:
            db.executescript(global_path.with_name("users.sql").read_text())
    with closing(sqlite3.connect(database)) as db:
        db.execute("PRAGMA foreign_keys = ON")
        with db:
            for user, account in legacy["accounts"].items():
                copy_account(db, user, account, defaults)
    if create:
        installation: dict[str, object] = {"environment": legacy["environment"], "database": str(database)}
        write_global_config(global_path, config_path, installation)
    binary = global_path.parent.parent / "bin" / "phone-assistant-server"
    for user, account in legacy["accounts"].items():
        for service in ("gmail", "calendar", "contacts"):
            key = f"PHONE_ASSISTANT_{service.upper()}_TOKEN_FILE"
            path = account["environment"].get(key, "")
            if path:
                status = subprocess.run(
                    [str(binary), "--connect-google", str(config_path), user, service,
                     str(Path(path).expanduser())], stdout=subprocess.DEVNULL, check=False,
                )
                if status.returncode:
                    return 1
    return 0


def write_global_config(template: Path, target: Path, installation: dict[str, object]) -> None:
    config = json.loads(template.read_text())
    config["installation"] = installation
    fd = os.open(target, os.O_WRONLY | os.O_CREAT | os.O_TRUNC | os.O_NOFOLLOW, 0o600)
    with os.fdopen(fd, "w") as stream:
        json.dump(config, stream)
        stream.write("\n")


def main(args: list[str]) -> int:
    try:
        match args:
            case ["prepare", global_file, source, config, database]:
                return migrate(Path(global_file), Path(source), Path(config), Path(database), True)
            case ["merge", global_file, installed, config]:
                current = json.loads(Path(installed).read_text())
                write_global_config(Path(global_file), Path(config), current["installation"])
                return 0
            case ["refresh", global_file, source, config]:
                target = json.loads(Path(config).read_text())["installation"]
                legacy = cast(Legacy, json.loads(Path(source).read_text()))
                if target["environment"] != legacy["environment"]:
                    print("Shared credentials changed after staging. Prepare the release again.", file=sys.stderr)
                    return 1
                return migrate(Path(global_file), Path(source), Path(config), Path(target["database"]), False)
            case _:
                return 2
    except (OSError, ValueError, KeyError, TypeError, AttributeError, sqlite3.Error):
        print("Could not migrate the existing account state.", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
