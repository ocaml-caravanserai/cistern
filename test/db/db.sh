#!/bin/sh
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at https://mozilla.org/MPL/2.0/.
#
# The test servers (DESIGN.md, "Database servers for the tests"). The Makefile
# calls this script from the root of the repository.
#
#   test/db/db.sh up NAME      Start the server NAME with a new data directory.
#   test/db/db.sh down [NAME]  Stop NAME, or each server that runs.
#   test/db/db.sh check NAME   The smoke check of the server NAME.
#   test/db/db.sh errors       Compare test/db/errors.tsv with perror.
#
# NAME is mysql84, mariadb114 or mariadb118. The variables:
#
#   DB_DIR          The root of the data (default: $PWD/.db).
#   DB_PORT         The TCP port of the server (default: 13384, 13414, 13418).
#   DB_SERVER_ARGS  More server options, for example --max-connections=50.
#   DUNE            The dune command (default: dune).
#
# The script is POSIX sh, so that it also runs on macOS.
set -eu

# The clients of MySQL 8.4 read ~/.mylogin.cnf also with --no-defaults, and
# all the clients read these variables. Thus the script removes them.
MYSQL_TEST_LOGIN_FILE=/dev/null
export MYSQL_TEST_LOGIN_FILE
unset MYSQL_PWD MYSQL_HOST MYSQL_TCP_PORT MYSQL_UNIX_PORT MYSQL_HOME

DB_DIR=${DB_DIR:-$PWD/.db}
# The server changes its directory to the data directory, thus each path must
# be absolute.
case $DB_DIR in
  /*) ;;
  *) DB_DIR=$PWD/$DB_DIR ;;
esac
DUNE=${DUNE:-dune}
NAMES="mysql84 mariadb114 mariadb118"
PASSWORD=cistern
DATABASE=cistern_test
# db.sh writes this file into each directory that it makes. It removes or
# uses a directory only if the file is there.
MARKER=.cistern-db

die() {
  echo "db.sh: $*" >&2
  exit 1
}

# The settings of the server $1.
server() {
  name=$1
  case $name in
    mysql84)
      family=mysql default_port=13384 user=cistern_sha2
      plugin=caching_sha2_password series=8.4.
      ;;
    mariadb114)
      family=mariadb default_port=13414 user=cistern_native
      plugin=mysql_native_password series=11.4.
      ;;
    mariadb118)
      family=mariadb default_port=13418 user=cistern_native
      plugin=mysql_native_password series=11.8.
      ;;
    *)
      echo "db.sh: unknown server '$name': give mysql84, mariadb114 or mariadb118" >&2
      exit 2
      ;;
  esac
  dir=${DB_DIR:?}/$name
  sock=$dir/s.sock
}

# The tools of the store path $store.
tools() {
  if [ "$family" = mysql ]; then
    mysqld=$store/bin/mysqld client=$store/bin/mysql
  else
    mysqld=$store/bin/mariadbd client=$store/bin/mariadb
  fi
}

# SQL as root on the socket, with no password. The MariaDB client asks for
# TLS by default, also on the socket. Thus it gets --skip-ssl, so that a
# server with no TLS (DB_SERVER_ARGS=--skip-ssl) can start.
root_sql() {
  if [ "$family" = mysql ]; then
    "$client" --no-defaults --protocol=SOCKET -uroot -S "$sock" -N -B -e "$1"
  else
    "$client" --no-defaults --protocol=SOCKET --skip-ssl -uroot -S "$sock" \
      -N -B -e "$1"
  fi
}

# True if the process $1 is a server of the directory $dir. A stale pid can
# name another process, or the server of another DB_DIR.
alive() {
  [ -n "$1" ] || return 1
  args=$(ps -ww -p "$1" -o args= 2>/dev/null) || return 1
  case $args in
    *mysqld*--pid-file=* | *mariadbd*--pid-file=*) ;;
    *) return 1 ;;
  esac
  # The pid file of the command line, up to the next option of up. The path
  # can have spaces. -ef compares the files, thus a trailing slash, a symlink
  # or a logical $PWD in DB_DIR gives the same result.
  file=${args#*--pid-file=}
  file=${file%% --log-error=*}
  [ "$file" -ef "$dir/pid" ]
}

# The pids that can name the server of $dir: the env file of db-up, and the
# pid file that the server writes early.
pids() {
  if [ -f "$dir/env" ]; then
    sed -n "s/^DB_PID='\([0-9]*\)'$/\1/p" "$dir/env"
  fi
  if [ -f "$dir/pid" ]; then
    cat "$dir/pid"
  fi
}

# Stop the server process $1 with SIGTERM. After 60 s, stop it with SIGKILL
# (the data is test data) and exit 1.
stop() {
  kill -TERM "$1" 2>/dev/null || true
  i=0
  while alive "$1"; do
    if [ "$i" -ge 120 ]; then
      kill -KILL "$1" 2>/dev/null || true
      die "the server (pid $1) did not stop after 60 s: sent SIGKILL"
    fi
    sleep 0.5
    i=$((i + 1))
  done
}

# On a failure or a signal after the start, stop the server, so that no
# server stays. dash runs the EXIT trap after a signal only if a trap of the
# signal calls exit.
pid=
cleanup() {
  status=$?
  if [ "$status" -ne 0 ] && alive "$pid"; then
    echo "db.sh: stop the server after the failure" >&2
    stop "$pid"
  fi
}

up() {
  server "$1"
  port=${DB_PORT:-$default_port}
  for old in $(pids); do
    if alive "$old"; then
      die "$name runs already: make db-down SERVER=$name"
    fi
  done
  # sun_path of a Unix socket has 104 bytes on macOS and 108 on Linux, with
  # the final NUL. Thus the path has at most 103 or 107 bytes.
  if [ "$(uname -s)" = Darwin ]; then limit=103; else limit=107; fi
  n=$(printf %s "$sock" | wc -c)
  n=$((n + 0))
  [ "$n" -le "$limit" ] ||
    die "the socket path $sock has $n bytes, the limit is $limit: set DB_DIR to a shorter directory"
  for d in "$dir" "$DB_DIR/certs"; do
    if [ -e "$d" ] && [ ! -f "$d/$MARKER" ]; then
      die "$d exists, and db.sh did not make it: remove it, or set DB_DIR"
    fi
  done
  store=$(nix build --no-link --print-out-paths ".#server-$name^out")
  tools
  [ -x "$mysqld" ] || die "no $mysqld"
  certs=$DB_DIR/certs
  if [ ! -f "$certs/ca.pem" ] || [ ! -f "$certs/server-cert.pem" ] ||
    [ ! -f "$certs/server-key.pem" ] ||
    [ ! -f "$certs/server-cert.sha256" ] || [ ! -f "$certs/other-ca.pem" ]; then
    # The check above makes sure that db.sh made $certs, if it exists.
    rm -rf "${DB_DIR:?}/certs"
    mkdir -p "$DB_DIR"
    $DUNE build test/certgen/certgen.exe
    # certgen makes the directory with mode 700.
    _build/default/test/certgen/certgen.exe "$certs"
    : >"$certs/$MARKER"
  fi
  rm -rf "${DB_DIR:?}/$name"
  # Mode 700: only the owner can use the socket of root.
  mkdir -m 700 "$dir"
  : >"$dir/$MARKER"
  mkdir "$dir/data"
  mkdir -m 700 "$dir/files"
  if [ "$family" = mysql ]; then
    "$mysqld" --no-defaults --basedir="$store" --datadir="$dir/data" \
      --initialize-insecure >"$dir/init.log" 2>&1 ||
      { cat "$dir/init.log" >&2; die "the initialization failed"; }
  else
    "$store/bin/mariadb-install-db" --no-defaults --basedir="$store" \
      --datadir="$dir/data" --auth-root-authentication-method=normal \
      --skip-test-db >"$dir/init.log" 2>&1 ||
      { cat "$dir/init.log" >&2; die "the initialization failed"; }
  fi
  extra=
  [ "$family" = mysql ] && extra=--mysqlx=OFF
  # mysqld refuses to run as root with no --user.
  [ "$(id -u)" -ne 0 ] || extra="$extra --user=root"
  trap cleanup EXIT
  trap 'exit 130' INT
  trap 'exit 143' TERM
  trap 'exit 129' HUP
  # $extra and $DB_SERVER_ARGS are lists of options. The server gets no
  # open file of this shell other than the three redirections: a lock or a
  # pipe of the caller (for example of nix) must not stay open.
  # shellcheck disable=SC2086
  "$mysqld" --no-defaults --basedir="$store" --datadir="$dir/data" \
    --bind-address=127.0.0.1 --skip-name-resolve --port="$port" \
    --socket="$sock" \
    --pid-file="$dir/pid" --log-error="$dir/error.log" \
    --secure-file-priv="$dir/files" --ssl-ca="$certs/ca.pem" \
    --ssl-cert="$certs/server-cert.pem" --ssl-key="$certs/server-key.pem" \
    $extra ${DB_SERVER_ARGS:-} </dev/null >"$dir/server.out" 2>&1 \
    3>&- 4>&- 5>&- 6>&- 7>&- 8>&- 9>&- &
  pid=$!
  # A query, not mysqladmin ping: ping gives status 0 also for "Access
  # denied". The errors 2002 and 2003 tell that the server is not ready yet.
  # Another error stops the wait.
  i=0
  until root_sql "SELECT 1" >/dev/null 2>"$dir/wait.err"; do
    if ! kill -0 "$pid" 2>/dev/null; then
      tail -n 20 "$dir/error.log" >&2 || true
      die "the server stopped"
    fi
    if grep -q '^ERROR' "$dir/wait.err" &&
      ! grep -Eq '^ERROR (2002|2003) ' "$dir/wait.err"; then
      cat "$dir/wait.err" >&2
      tail -n 20 "$dir/error.log" >&2 || true
      die "the client of root failed"
    fi
    if [ "$i" -ge 120 ]; then
      cat "$dir/wait.err" >&2
      tail -n 20 "$dir/error.log" >&2 || true
      die "no connection after 60 s"
    fi
    sleep 0.5
    i=$((i + 1))
  done
  rm -f "$dir/wait.err"
  # Root keeps only the account root@localhost, which with
  # --skip-name-resolve matches only the socket (mode 700). MariaDB also
  # makes root at 127.0.0.1, ::1 and the host name, with no password.
  others=$(root_sql "SELECT CONCAT(QUOTE(user), '@', QUOTE(host))
    FROM mysql.user WHERE user = 'root' AND host <> 'localhost'")
  for account in $others; do
    root_sql "DROP USER $account"
  done
  if [ "$family" = mysql ]; then
    ident="IDENTIFIED WITH caching_sha2_password BY '$PASSWORD'"
  else
    ident="IDENTIFIED VIA mysql_native_password USING PASSWORD('$PASSWORD')"
  fi
  # Host %: the bind address keeps the server local.
  root_sql "CREATE DATABASE $DATABASE;
    CREATE USER '$user'@'%' $ident;
    GRANT ALL ON $DATABASE.* TO '$user'@'%';
    GRANT RELOAD ON *.* TO '$user'@'%';"
  version=$(root_sql "SELECT @@version")
  {
    echo "# make db-up SERVER=$name. The admin route: DB_ADMIN_USER on"
    echo "# DB_SOCKET, with no password."
    for v in "DB_SERVER=$name" "DB_STORE=$store" "DB_VERSION=$version" \
      "DB_PORT=$port" "DB_SOCKET=$sock" "DB_USER=$user" \
      "DB_PASSWORD=$PASSWORD" "DB_DATABASE=$DATABASE" \
      "DB_CA=$certs/ca.pem" "DB_FINGERPRINT=$(cat "$certs/server-cert.sha256")" \
      "DB_OTHER_CA=$certs/other-ca.pem" "DB_PLUGIN=$plugin" "DB_ADMIN_USER=root" \
      "DB_PID=$pid"; do
      printf "%s='%s'\n" "${v%%=*}" "${v#*=}"
    done
  } >"$dir/env"
  echo "$name $version on 127.0.0.1:$port and $sock"
}

# Run a step of check: $1 is the name of the step, $2 the expected output
# (or "ERROR <n>" for an expected error), then the command. The step prints
# "ok", or "FAIL" and exits.
step() {
  label=$1 expected=$2
  shift 2
  if out=$("$@" 2>"$DB_DIR/check.err"); then
    got=$out
  else
    rc=$?
    got=$(grep -o '^ERROR [0-9]*' "$DB_DIR/check.err" || echo "status $rc")
  fi
  if [ "$got" = "$expected" ]; then
    echo "ok $label"
  else
    echo "FAIL $label"
    echo "  expected: $expected"
    echo "  actual:   $got"
    grep -v 'password on the command line' "$DB_DIR/check.err" | sed 's/^/  /'
    exit 1
  fi
}

# The test user over TCP with the client $1 and the options that follow.
tcp() {
  c=$1
  shift
  "$c" --no-defaults --protocol=TCP -h127.0.0.1 -P"$DB_PORT" -u"$DB_USER" \
    --password="$DB_PASSWORD" -N -B "$@"
}

# The test user on the socket with the client of the server.
sock_sql() {
  "$client" --no-defaults --protocol=SOCKET -S "$DB_SOCKET" -u"$DB_USER" \
    --password="$DB_PASSWORD" -N -B -e "$1"
}

check() {
  server "$1"
  [ -f "$dir/env" ] || die "$name is not started: make db-up SERVER=$name"
  # shellcheck disable=SC1091
  . "$dir/env"
  alive "$DB_PID" || die "$name does not run: make db-up SERVER=$name"
  store=$DB_STORE
  sock=$DB_SOCKET
  tools
  case $DB_VERSION in
    "$series"*) echo "ok version $DB_VERSION" ;;
    *) die "FAIL version: $DB_VERSION is not $series*" ;;
  esac
  mysql84=$(nix build --no-link --print-out-paths '.#server-mysql84^out')/bin/mysql
  me="$DB_VERSION	$DB_USER@%"
  q="SELECT @@version, CURRENT_USER()"
  if [ "$family" = mysql ]; then
    # The paths of caching_sha2_password: with no TLS, only a user in the
    # cache of the server passes (fast path). The full path fills the cache,
    # on the socket or with TLS. FLUSH PRIVILEGES (RELOAD) empties it.
    root_sql "FLUSH PRIVILEGES"
    off=--ssl-mode=DISABLED
    step "sha2: no TLS, empty cache" "ERROR 2061" tcp "$client" "$off" -e "$q"
    step "sha2: socket, full path" "$me" sock_sql "$q"
    step "sha2: no TLS, fast path" "$me" tcp "$client" "$off" -e "$q"
    step "sha2: FLUSH PRIVILEGES as the test user" "" sock_sql "FLUSH PRIVILEGES"
    step "sha2: no TLS after the flush" "ERROR 2061" tcp "$client" "$off" -e "$q"
    step "sha2: TLS, full path" "$me" tcp "$client" \
      --ssl-mode=VERIFY_IDENTITY --ssl-ca="$DB_CA" -e "$q"
    step "sha2: no TLS, fast path again" "$me" tcp "$client" "$off" -e "$q"
    verify=--ssl-mode=VERIFY_IDENTITY
  else
    # The MariaDB client checks the chain of the certificate, but not the
    # name or the IP address of a connection to 127.0.0.1. Step "identity"
    # checks them.
    verify=--ssl-verify-server-cert
  fi
  step "TLS with the client of the server" "$me
Ssl_version	TLSv1.3" tcp "$client" "$verify" --ssl-ca="$DB_CA" \
    -e "$q; SHOW SESSION STATUS LIKE 'Ssl_version'"
  step "identity of the certificate (the MySQL 8.4 client)" "$me" \
    tcp "$mysql84" --ssl-mode=VERIFY_IDENTITY --ssl-ca="$DB_CA" -e "$q"
  step "socket" "$me" sock_sql "$q"
  step "plugin" "$DB_PLUGIN" root_sql \
    "SELECT plugin FROM mysql.user WHERE user = '$DB_USER'"
  if [ "$family" = mysql ]; then
    origin="SELECT VARIABLE_NAME FROM performance_schema.variables_info
      WHERE VARIABLE_SOURCE NOT IN ('COMPILED', 'COMMAND_LINE', 'DYNAMIC')"
  else
    origin="SELECT VARIABLE_NAME FROM information_schema.SYSTEM_VARIABLES
      WHERE GLOBAL_VALUE_ORIGIN = 'CONFIG'"
  fi
  step "no variable from a defaults file" "" root_sql "$origin"
  rm -f "$DB_DIR/check.err"
}

# The name of the error $2 from perror of the store path $1, or nothing.
# MySQL: "MySQL error code MY-001062 (ER_DUP_ENTRY): ...". MariaDB: "MariaDB
# error code 1062 (ER_DUP_ENTRY): ...", with status 0 also for a number that
# it does not know.
error_name() {
  "$1/bin/perror" "$2" 2>/dev/null |
    sed -n 's/^[A-Za-z]* error code [A-Z0-9-]* (\([A-Z0-9_]*\)):.*/\1/p' |
    head -n 1
}

errors() {
  my=$(nix build --no-link --print-out-paths '.#server-mysql84^out')
  m114=$(nix build --no-link --print-out-paths '.#server-mariadb114^out')
  m118=$(nix build --no-link --print-out-paths '.#server-mariadb118^out')
  bad=0 rows=0
  tab=$(printf '\t')
  while IFS=$tab read -r dialect number expected; do
    case $dialect in
      '#'* | '') continue ;;
      mysql) stores=$my ;;
      mariadb) stores="$m114 $m118" ;;
      *) die "errors.tsv: unknown dialect '$dialect'" ;;
    esac
    rows=$((rows + 1))
    # perror of a new package can give no name for a number that it does not
    # know. Thus an empty name in the file is a failure, not a match.
    if [ -z "$expected" ]; then
      echo "FAIL $dialect $number: errors.tsv has no name"
      bad=$((bad + 1))
      continue
    fi
    for s in $stores; do
      got=$(error_name "$s" "$number")
      if [ "$got" != "$expected" ]; then
        echo "FAIL $dialect $number: expected $expected, perror of ${s#*-} gives '${got}'"
        bad=$((bad + 1))
      fi
    done
  done <test/db/errors.tsv
  [ "$bad" -eq 0 ] || die "$bad names differ"
  echo "ok $rows error names"
}

down() {
  for n in ${1:-$NAMES}; do
    server "$n"
    if [ ! -f "$dir/env" ] && [ ! -f "$dir/pid" ]; then
      [ -z "${1:-}" ] || echo "$name: not started"
      continue
    fi
    stopped=
    for p in $(pids); do
      if alive "$p"; then
        stop "$p"
        stopped=1
      fi
    done
    if [ -n "$stopped" ]; then
      echo "$name: stopped"
    else
      echo "$name: not running"
    fi
  done
}

case ${1:-} in
  up)
    [ $# -eq 2 ] || die "usage: db.sh up NAME"
    up "$2"
    ;;
  down)
    [ $# -le 2 ] || die "usage: db.sh down [NAME]"
    down "${2:-}"
    ;;
  check)
    [ $# -eq 2 ] || die "usage: db.sh check NAME"
    check "$2"
    ;;
  errors)
    [ $# -eq 1 ] || die "usage: db.sh errors"
    errors
    ;;
  *)
    echo "usage: db.sh up NAME | check NAME | down [NAME] | errors" >&2
    exit 2
    ;;
esac
