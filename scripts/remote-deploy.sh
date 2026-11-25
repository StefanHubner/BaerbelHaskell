#!/usr/bin/env bash
set -euo pipefail
action=$1
work=$(cd "$(dirname "$0")" && pwd)
prefix=/home/stefan/apps/baerbel
service=phone-assistant.service
override=/etc/systemd/system/phone-assistant.service.d/zz-baerbel-haskell.conf
user_config=/etc/phone-assistant/user.json
global_config=/etc/phone-assistant/global.yaml
legacy_global_config=/etc/phone-assistant/global.json
pbx_config=/etc/phone-assistant/asterisk.yaml
pbx_directory=/etc/asterisk
legacy_routes=/etc/phone-assistant/account-routes
user_database=/var/lib/phone-assistant/users.sqlite3
legacy_state="$prefix/legacy"
legacy_env=/etc/phone-assistant/assistant.env
legacy_env_backup=/var/backups/phone-assistant-legacy/assistant.env

prepare_release() {
  local digest release stage
  local -a config
  digest=$(sha256sum "$work/release.tar.gz")
  digest=${digest%% *}
  release="$prefix/releases/$digest"
  mkdir -p "$prefix/releases"
  if [[ ! -d "$release" ]]; then
    stage=$(mktemp -d "$prefix/releases/.incoming.XXXXXXXX")
    tar --no-same-owner --no-same-permissions -xzf "$work/release.tar.gz" -C "$stage"
    mv "$stage/baerbel" "$release"
    rmdir "$stage"
  fi
  install_config_assets "$release" "$work"
  prepare_config "$release/config/global.yaml"
  env -u LD_PRELOAD ldd "$release/bin/phone-assistant-server" "$release/bin/asteriskconfig"
  if [[ -f "$work/global.yaml" && -f "$release/deploy/migrate-users.py" ]]; then
    config=("$work/global.yaml")
  else
    config=("$release/config/global.yaml" "$work/user.json")
  fi
  "$release/bin/phone-assistant-server" --check "${config[@]}"
  ln -sfn "releases/$digest" "$prefix/staged"
  printf 'Prepared and checked release: %s\nThe running service has not changed.\n' "$release"
}

install_config_assets() {
  local release=$1 target=$2
  install -d -m 0755 "$target/tools" || return
  install -m 0644 "$release/config/instructions.yaml" "$release/config/phone-numbers.json" "$target/" || return
  install -m 0644 "$release/config/tools/"*.yaml "$target/tools/"
}

prepare_config() {
  local global=$1 migration installed
  migration=$(dirname "$global")/../deploy/migrate-users.py
  if [[ -f "$global_config" || -f "$legacy_global_config" ]]; then
    installed=$global_config
    [[ -f "$installed" ]] || installed=$legacy_global_config
    python3 "$migration" merge "$global" "$installed" "$work/global.yaml"
  elif [[ -f "$user_config" && -f "$migration" ]]; then
    python3 "$migration" prepare "$global" "$user_config" "$work/global.yaml" "$user_database"
  elif [[ -f "$user_config" ]]; then
    install -m 0600 "$user_config" "$work/user.json"
  else
    python3 -c 'import json, sys, yaml; print(json.dumps(yaml.safe_load(sys.stdin)))' < "$global" |
      /home/stefan/.venvs/assistant/bin/python "$work/prepare-user-config.py" "$legacy_env" "$work/user.json" /dev/stdin
  fi
}

require_idle() {
  local channels
  channels=$(asterisk -rx 'core show channels concise') || return
  asterisk -rx 'core show uptime' | grep -F 'System uptime:' >/dev/null || return
  [[ -z "$channels" ]] || { printf 'Asterisk has active calls; the service was not restarted.\n' >&2; return 1; }
}

activate_release() {
  local release service_user unit
  local -a config active_config
  local prepared_user=${1:-"$work/user.json"} target_config=${2:-"$user_config"}
  require_idle || return
  release=$(readlink -e "$prefix/staged") || return
  service_user=$(systemctl show "$service" --property=User --value) || return
  if [[ "$target_config" == "$global_config" ]]; then
    config=("$prepared_user")
    active_config=("$target_config")
    install_config_assets "$release" "${global_config%/*}" || return
  else
    config=("$release/config/global.yaml" "$prepared_user")
    active_config=("$release/config/global.yaml" "$target_config")
  fi
  runuser -u "$service_user" -- "$release/bin/phone-assistant-server" --check "${config[@]}" || return
  install -m 0600 -o "$service_user" -g "$service_user" "$prepared_user" "$target_config" || return
  unit=$(mktemp) || return
  cat > "$unit" <<UNIT
# Managed by Bärbel Haskell deployment; remove this file to restore Python.
[Service]
WorkingDirectory=$release
ExecStart=
ExecStart=$release/bin/phone-assistant-server ${active_config[*]}
UNIT
  install -D -m 0644 "$unit" "$override" || return
  rm "$unit"
  systemctl daemon-reload || return
  systemctl restart "$service" || return
  verify_service "$release/bin/phone-assistant-server"
}

activate_prepared() {
  local release
  release=$(readlink -e "$prefix/staged") || return
  if [[ -f "$global_config" && -f "$pbx_config" ]]; then
    activate_release "$work/global.yaml" "$global_config"
  elif [[ -f "$legacy_global_config" && -f "$legacy_routes" ]]; then
    activate_pbx_database "$release"
  elif [[ -f "$global_config" || -f "$legacy_global_config" ]]; then
    activate_release "$work/global.yaml" "$global_config" || return
    move_legacy "$legacy_global_config" "${legacy_env_backup%/*}/global.json"
  elif [[ -f "$user_database" && -f "$release/deploy/migrate-users.py" ]]; then
    activate_user_database "$release"
  else
    activate_release
  fi
}

activate_user_database() {
  local release=$1 service_user backup
  require_idle || return
  asterisk -rx 'dialplan show baerbel-account-entry' | grep -F 'agi://127.0.0.1:4573/account' >/dev/null || return
  service_user=$(systemctl show "$service" --property=User --value) || return
  runuser -u "$service_user" -- "$release/bin/phone-assistant-server" --check "$work/global.yaml" || return
  backup=$(mktemp -d /var/backups/phone-assistant-users.XXXXXXXX) || return
  cp -a "$override" "$user_config" "$backup/" || return
  systemctl stop "$service" || return
  if runuser -u "$service_user" -- python3 "$release/deploy/migrate-users.py" refresh \
    "$release/config/global.yaml" "$user_config" "$work/global.yaml" &&
    activate_release "$work/global.yaml" "$global_config"; then
    rm "$user_config" || printf 'The old account JSON is still available beside the global configuration.\n' >&2
    printf 'User database active. Previous private configuration and unit saved at %s.\n' "$backup"
    return
  fi
  cp -a "$backup/zz-baerbel-haskell.conf" "$override"
  rm -f "$global_config"
  systemctl daemon-reload
  systemctl restart "$service"
  printf 'User database activation stopped; the previous service is running.\n' >&2
  return 1
}

activate_accounts() {
  local release route candidate backup service_user shared dedicated account
  require_idle || return
  release=$(readlink -e "$prefix/staged")
  route=/etc/asterisk/phone-assistant-extensions.conf
  service_user=$(systemctl show "$service" --property=User --value)
  runuser -u "$service_user" -- "$release/bin/phone-assistant-server" --check "$release/config/global.yaml" "$work/accounts.json"
  read -r shared dedicated account < "$work/account-routes"
  candidate=$(mktemp)
  python3 "$release/deploy/asterisk/render-asterisk-accounts.py" "$route" \
    "$release/deploy/asterisk/account-entry.conf.in" "$shared" "$dedicated" "$account" "$candidate"
  backup=$(mktemp -d /var/backups/phone-assistant-accounts.XXXXXXXX)
  cp -a "$route" "$user_config" "$override" "$backup/"
  if install -o root -g asterisk -m 0640 "$candidate" "$route" &&
    asterisk -rx 'dialplan reload' &&
    asterisk -rx 'dialplan show baerbel-account-entry' | grep -F '[ Context' >/dev/null &&
    activate_release "$work/accounts.json"; then
    install -m 0600 "$work/account-routes" /etc/phone-assistant/account-routes
    enable_routing_trace
    rm "$candidate"
    printf 'Account routes installed. Backup: %s\n' "$backup"
    return
  fi
  cp -a "$backup/phone-assistant-extensions.conf" "$route"
  cp -a "$backup/user.json" "$user_config"
  cp -a "$backup/zz-baerbel-haskell.conf" "$override"
  asterisk -rx 'dialplan reload'
  systemctl daemon-reload
  systemctl restart "$service"
  rm "$candidate"
  printf 'Account activation failed; restored previous configuration.\n' >&2
  return 1
}

enable_routing_trace() {
  local file=/var/lib/phone-assistant-incoming-events/account-routing.log
  [[ -e "$file" ]] || install -o asterisk -g stefan -m 0640 /dev/null "$file"
  asterisk -rx 'database put baerbel account-routing-trace 1'
}

activate_pbx_database() {
  local release=$1 service_user backup
  local installer="$release/bin/asteriskconfig"
  require_idle || return
  service_user=$(systemctl show "$service" --property=User --value) || return
  runuser -u "$service_user" -- "$release/bin/phone-assistant-server" --check "$work/global.yaml" || return
  backup=$(mktemp -d /var/backups/baerbel-asterisk.XXXXXXXX) || return
  cp -a "$override" "$legacy_routes" "$legacy_global_config" "$backup/" || return
  preflight_pbx_database "$installer" "$backup" || return
  systemctl stop "$service" || return
  if ! cp -a "$user_database" "$backup/users.sqlite3"; then
    systemctl start "$service"
    return 1
  fi
  if install_pbx_database "$installer" "$backup" "$release" "$service_user"; then
    archive_pbx_inputs "$backup" || return
    printf 'Asterisk configuration installed from YAML and SQLite. Backup %s\n' "$backup"
    return
  fi
  systemctl stop "$service" || return
  cp -a "$backup/users.sqlite3" "$user_database"
  cp -a "$backup/zz-baerbel-haskell.conf" "$override"
  rm -f "$global_config" "$pbx_config"
  systemctl daemon-reload
  systemctl start "$service"
  return 1
}

preflight_pbx_database() {
  local installer=$1 backup=$2
  local candidate="$backup/preview.sqlite3"
  python3 -c '
import sqlite3, sys
from contextlib import closing
with closing(sqlite3.connect("file:" + sys.argv[1] + "?mode=ro", uri=True)) as live:
    with closing(sqlite3.connect(sys.argv[2])) as copy:
        live.backup(copy)
' "$user_database" "$candidate" || return
  "$installer" adopt "$pbx_directory" "$legacy_routes" "$backup/preview.yaml" "$candidate" || return
  "$installer" prepare "$backup/preview.yaml" "$candidate" "$backup/preview" || return
  rm -r -- "$backup/preview" "$backup/preview.yaml" "$candidate"
}

install_pbx_database() {
  local installer=$1 backup=$2 release=$3 service_user=$4
  "$installer" adopt "$pbx_directory" "$legacy_routes" "$backup/asterisk.yaml" "$user_database" || return
  runuser -u "$service_user" -- "$release/bin/phone-assistant-server" --check "$work/global.yaml" || return
  install -m 0600 "$backup/asterisk.yaml" "$pbx_config" || return
  "$installer" install "$pbx_config" "$user_database" "$backup/asterisk" || return
  activate_release "$work/global.yaml" "$global_config" && return
  "$installer" restore "$pbx_directory" "$backup/asterisk"
  return 1
}

install_account_routing() {
  local source=$1 release backup result service_user
  release=$(readlink -e "$prefix/staged") || return
  service_user=$(stat -c %G "$user_database") || return
  install_pbx_directories "$service_user" || return
  backup=$(mktemp -d /var/backups/baerbel-asterisk.XXXXXXXX) || return
  if ! stage_pbx_source "$source" "$backup"; then
    return 1
  fi
  if ! result=$("$release/bin/asteriskconfig" install "$pbx_config" "$user_database" "$release/config/asterisk" "$backup/asterisk"); then
    restore_pbx_source "$backup"
    return 1
  fi
  printf '%s\n' "$result"
  if [[ "$source" == "$work/asterisk-source.yaml" ]]; then rm -f -- "$source"; fi
  archive_pbx_inputs "$backup" || return
  rmdir -- "$backup" 2>/dev/null || true
  if [[ "$result" == "Asterisk configuration updated." ]] && systemctl is-active --quiet "$service"; then
    systemctl restart "$service" || return
    verify_service
  fi
}

install_pbx_directories() {
  local group=$1 recordings=/var/lib/phone-assistant-call-recordings events=/var/lib/phone-assistant-incoming-events
  [[ -d "$recordings" ]] || install -d -o asterisk -g "$group" -m 2750 "$recordings" || return
  [[ -d "$events" ]] || install -d -o asterisk -g "$group" -m 2750 "$events"
}

stage_pbx_source() {
  local source=$1 backup=$2
  cmp -s "$source" "$pbx_config" && return 0
  if [[ -f "$pbx_config" ]]; then
    cp -a -- "$pbx_config" "$backup/asterisk.yaml" || return
  fi
  install -D -m 0600 -- "$source" "$pbx_config" && touch "$backup/site-changed"
}

restore_pbx_source() {
  local backup=$1
  [[ -f "$backup/site-changed" ]] || return 0
  if [[ -f "$backup/asterisk.yaml" ]]; then
    cp -a -- "$backup/asterisk.yaml" "$pbx_config"
  else
    rm -f -- "$pbx_config"
  fi
}

archive_pbx_inputs() {
  local backup=$1
  move_legacy "$legacy_global_config" "$backup/active-global.json" || return
  move_legacy "$legacy_routes" "$backup/legacy-account-routes" || return
  move_legacy "$work/account-routes" "$backup/staged-account-routes" || return
  move_legacy "$work/global.json" "$backup/staged-global.json" || return
  move_legacy /etc/phone-assistant/telematica.env "$backup/telematica.env" || return
  move_legacy /home/stefan/.config/phone-assistant/telematica.pending.env "$backup/telematica.pending.env"
}

move_legacy() {
  local source=$1 destination=$2
  [[ -e "$source" ]] || return 0
  [[ ! -e "$destination" ]] || { printf 'Both copies exist: %s\n' "$destination" >&2; return 1; }
  mkdir -p -m 0700 -- "${destination%/*}" || return
  mv -- "$source" "$destination"
}

move_legacy_state() {
  local from=$1 to=$2 file
  for file in \
    var/lib/phone-assistant/{calendar-events.sqlite3,gmail-sends.sqlite3,incoming-events-cursor.sqlite3,sms-sends.sqlite3,profile.json,profile.json.lock} \
    home/stefan/.config/phone-assistant/{contacts-token,gmail-token-all-calendars,gmail-token-calendar,gmail-token}.json \
    home/stefan/.cache/baerbel-hs/deploy/{accounts,user}.json; do
    move_legacy "$from/$file" "$to/$file" || return
  done
}

require_live_database() {
  local pid executable
  pid=$(systemctl show "$service" --property=MainPID --value) || return
  executable=$(readlink -e "/proc/$pid/exe") || return
  [[ "$executable" == "$prefix"/releases/*/bin/phone-assistant-server && -f "$user_database" ]] || return
  grep -zxF -- "$global_config" "/proc/$pid/cmdline" >/dev/null
}

archive_legacy_state() {
  require_live_database || { printf 'The database release must be running.\n' >&2; return 1; }
  install -d -m 0700 "$legacy_state" || return
  move_legacy_state "" "$legacy_state" || return
  printf 'Legacy account state archived under %s\n' "$legacy_state"
}

archive_legacy_env() {
  require_live_database || { printf 'The database release must be running.\n' >&2; return 1; }
  move_legacy "$legacy_env" "$legacy_env_backup" || return
  printf 'Legacy Python environment archived at %s\n' "$legacy_env_backup"
}

restore_legacy() {
  local service_user
  service_user=$(systemctl show "$service" --property=User --value) || return
  runuser -u "$service_user" -- bash "$work/remote-deploy.sh" restore-legacy-state || return
  move_legacy "$legacy_env_backup" "$legacy_env"
}

rollback_accounts() {
  local backup route previous
  require_idle
  backup=$(find /var/backups -maxdepth 1 -type d -name 'phone-assistant-accounts.*' -printf '%T@ %p\n' |
    sort -nr | sed -n '1s/^[^ ]* //p')
  [[ -n "$backup" && -f "$backup/zz-baerbel-haskell.conf" && -f "$backup/user.json" &&
     -f "$backup/phone-assistant-extensions.conf" ]] || {
    printf 'The pre-cutover backup was not found.\n' >&2
    return 1
  }
  previous=$(sed -n 's|^ExecStart=\(/home/stefan/apps/baerbel/releases/[^ ]*/bin/phone-assistant-server\) .*|\1|p' "$backup/zz-baerbel-haskell.conf")
  [[ -x "$previous" ]] || { printf 'The previous Haskell executable is missing.\n' >&2; return 1; }
  restore_legacy
  route=/etc/asterisk/phone-assistant-extensions.conf
  python3 "$work/account-incident.py" || printf 'Optional routing trace was unavailable.\n' >&2
  cp -a "$backup/phone-assistant-extensions.conf" "$route"
  cp -a "$backup/user.json" "$user_config"
  cp -a "$backup/zz-baerbel-haskell.conf" "$override"
  asterisk -rx 'dialplan reload'
  systemctl daemon-reload
  systemctl restart "$service"
  verify_service "$previous"
  rm -f /etc/phone-assistant/account-routes
}

rollback_release() {
  local python
  python=$(readlink -e /home/stefan/.venvs/assistant/bin/python)
  require_idle
  restore_legacy
  rm -f "$override"
  systemctl daemon-reload
  systemctl restart "$service"
  verify_service "$python"
}

verify_service() {
  local expected=${1:-} first_pid current_pid
  first_pid=$(systemctl show "$service" --property=MainPID --value)
  verify_running "$expected" "$first_pid" || return
  sleep 3
  current_pid=$(systemctl show "$service" --property=MainPID --value)
  [[ "$current_pid" == "$first_pid" ]] || { printf 'Assistant restarted during startup verification.\n' >&2; return 1; }
  verify_running "$expected" "$current_pid" || return
  systemctl --no-pager show "$service" --property=ExecStart --property=MainPID
  printf 'Process checks passed; real call behavior has not been exercised.\n'
}

verify_running() {
  local expected=${1:-} pid=$2 executable
  systemctl is-active "$service" || return
  systemctl is-active asterisk.service || return
  [[ "$pid" =~ ^[1-9][0-9]*$ ]] || { printf 'Assistant has no running main process.\n' >&2; return 1; }
  executable=$(readlink -e "/proc/$pid/exe")
  [[ -z "$expected" || "$executable" == "$expected" ]] || { printf 'Assistant is not running the staged executable.\n' >&2; return 1; }
  printf 'Running executable: %s\n' "$executable"
}

case "$action" in
  prepare) prepare_release ;;
  activate) activate_prepared ;;
  activate-accounts) activate_accounts ;;
  install-account-routing) install_account_routing "$2" ;;
  rollback-accounts) rollback_accounts ;;
  rollback) rollback_release ;;
  archive-legacy-state) archive_legacy_state ;;
  archive-legacy-env) archive_legacy_env ;;
  restore-legacy-state) move_legacy_state "$legacy_state" "" ;;
esac
