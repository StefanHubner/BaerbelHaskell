#!/usr/bin/env bash
set -euo pipefail
action=${1:?Expected install, activate or rollback}
prefix=${2:?Expected absolute installation prefix}
archive=${3:-}
service=phone-assistant.service
override=/etc/systemd/system/phone-assistant.service.d/zz-baerbel-haskell.conf
user_config=/etc/phone-assistant/user.json
# These paths become literal systemd configuration, not shell expressions.
[[ "$prefix" =~ ^/[a-zA-Z0-9/_-]+$ ]] || { printf 'Invalid installation prefix\n' >&2; exit 2; }

install_release() {
  local digest release stage
  digest=$(sha256sum "$archive")
  digest=${digest%% *}
  release="$prefix/releases/$digest"
  mkdir -p "$prefix/releases"
  if [[ ! -d "$release" ]]; then
    stage=$(mktemp -d "$prefix/releases/.incoming.XXXXXXXX")
    tar --no-same-owner --no-same-permissions -xzf "$archive" -C "$stage"
    mv "$stage/baerbel" "$release"
    rmdir "$stage"
  fi
  ln -sfn "releases/$digest" "$prefix/staged"
  printf 'Staged release: %s\nThe running service has not changed.\n' "$release"
}

activate_release() {
  local release service_user unit
  release=$(readlink -e "$prefix/staged")
  service_user=$(systemctl show "$service" --property=User --value)
  runuser -u "$service_user" -- "$release/bin/phone-assistant-server" --check "$release/config/global.json" "$user_config"
  unit=$(mktemp)
  cat > "$unit" <<UNIT
# Managed by Bärbel Haskell deployment; remove this file to restore Python.
[Service]
WorkingDirectory=$release
ExecStart=
ExecStart=$release/bin/phone-assistant-server $release/config/global.json $user_config
UNIT
  install -D -m 0644 "$unit" "$override"
  rm "$unit"
  systemctl daemon-reload
  systemctl restart "$service"
  verify_service
}

rollback_release() {
  rm -f "$override"
  systemctl daemon-reload
  systemctl restart "$service"
  verify_service
}

verify_service() {
  systemctl is-active "$service"
  systemctl is-active asterisk.service
  systemctl show "$service" --property=ExecStart --property=MainPID
  printf 'Process checks passed; real call behavior has not been exercised.\n'
}

case "$action" in
  install) install_release ;;
  activate) activate_release ;;
  rollback) rollback_release ;;
  *) printf 'Unknown deployment action: %s\n' "$action" >&2; exit 2 ;;
esac
