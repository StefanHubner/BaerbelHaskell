PRAGMA foreign_keys = ON;
PRAGMA user_version = 1;

CREATE TABLE user_types (
    name TEXT PRIMARY KEY CHECK (name IN ('Standard', 'Pro'))
) WITHOUT ROWID;

INSERT INTO user_types VALUES ('Standard'), ('Pro');

CREATE TABLE users (
    id TEXT PRIMARY KEY,
    user_type TEXT NOT NULL REFERENCES user_types(name),
    name TEXT NOT NULL,
    phone TEXT NOT NULL UNIQUE,
    pin_hash TEXT NOT NULL,
    memory_updated_at TEXT
) WITHOUT ROWID;

CREATE TABLE user_settings (
    user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    name TEXT NOT NULL CHECK (name <> 'PHONE_ASSISTANT_PRIVATE_PIN_HASH'),
    value TEXT NOT NULL,
    PRIMARY KEY (user_id, name)
) WITHOUT ROWID;

CREATE TABLE oauth_clients (
    client_id TEXT PRIMARY KEY,
    client_secret TEXT NOT NULL
) WITHOUT ROWID;

CREATE TABLE google_grants (
    user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    grant_id INTEGER NOT NULL,
    client_id TEXT NOT NULL REFERENCES oauth_clients(client_id),
    refresh_token TEXT NOT NULL,
    PRIMARY KEY (user_id, grant_id),
    UNIQUE (user_id, client_id, refresh_token)
) WITHOUT ROWID;

CREATE TABLE google_grant_scopes (
    user_id TEXT NOT NULL,
    grant_id INTEGER NOT NULL,
    scope TEXT NOT NULL,
    PRIMARY KEY (user_id, grant_id, scope),
    FOREIGN KEY (user_id, grant_id) REFERENCES google_grants(user_id, grant_id) ON DELETE CASCADE
) WITHOUT ROWID;

CREATE TABLE google_connections (
    user_id TEXT NOT NULL,
    service TEXT NOT NULL CHECK (service IN ('gmail', 'calendar', 'contacts')),
    grant_id INTEGER NOT NULL,
    PRIMARY KEY (user_id, service),
    FOREIGN KEY (user_id, grant_id) REFERENCES google_grants(user_id, grant_id) ON DELETE CASCADE
) WITHOUT ROWID;

CREATE TABLE user_memory (
    user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    key TEXT NOT NULL,
    value TEXT NOT NULL,
    PRIMARY KEY (user_id, key)
) WITHOUT ROWID;

CREATE TABLE delivery_receipts (
    user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    service TEXT NOT NULL CHECK (service IN ('gmail', 'calendar')),
    request_id TEXT NOT NULL,
    fingerprint TEXT NOT NULL,
    result TEXT NOT NULL,
    created_at TEXT NOT NULL,
    PRIMARY KEY (user_id, service, request_id)
) WITHOUT ROWID;

CREATE TABLE sms_receipts (
    user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    request_id TEXT NOT NULL,
    fingerprint TEXT NOT NULL,
    status TEXT NOT NULL,
    created_at TEXT NOT NULL,
    kind TEXT NOT NULL,
    fallback INTEGER NOT NULL,
    PRIMARY KEY (user_id, request_id)
) WITHOUT ROWID;

CREATE TABLE event_cursors (
    user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    file_key TEXT NOT NULL,
    position INTEGER NOT NULL,
    anchor_length INTEGER NOT NULL,
    anchor TEXT NOT NULL,
    PRIMARY KEY (user_id, file_key)
) WITHOUT ROWID;

CREATE TABLE sip_accounts (
    number TEXT PRIMARY KEY,
    username TEXT NOT NULL,
    password TEXT NOT NULL,
    pro_user_id TEXT UNIQUE REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE
) WITHOUT ROWID;

CREATE UNIQUE INDEX one_shared_sip ON sip_accounts ((1)) WHERE pro_user_id IS NULL;

CREATE VIEW user_sip_accounts AS
SELECT users.id AS user_id, sip_accounts.number AS sip_number
FROM users JOIN sip_accounts
ON (users.user_type = 'Standard' AND sip_accounts.pro_user_id IS NULL)
OR (users.user_type = 'Pro' AND sip_accounts.pro_user_id = users.id);
