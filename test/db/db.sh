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
      plugin=caching_sha2_password
      ;;
    mariadb114 | mariadb118)
      family=mariadb user=cistern_native plugin=mysql_native_password
      if [ "$name" = mariadb114 ]; then default_port=13414; else default_port=13418; fi
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
    [ ! -f "$certs/server-key.pem" ]; then
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
      "DB_CA=$certs/ca.pem" "DB_PLUGIN=$plugin" "DB_ADMIN_USER=root" \
      "DB_PID=$pid"; do
      printf "%s='%s'\n" "${v%%=*}" "${v#*=}"
    done
  } >"$dir/env"
  echo "$name $version on 127.0.0.1:$port and $sock"
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
  *)
    echo "usage: db.sh up NAME | down [NAME]" >&2
    exit 2
    ;;
esac
