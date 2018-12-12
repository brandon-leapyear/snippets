#!/bin/bash
#
# Run a postgres server, pause, then clean up

set -e -o pipefail
trap ': ${e=${?}}; (( e > 0 )) && echo "${BASH_SOURCE}: exit ${e}" 1>&2' EXIT

usage='"
Usage: ./run_postgres.sh [--help] [--restore FILE] [--database|-d DB]

Run a PostgreSQL server, wait for input, then clean up.

Options:
--help          Show this usage text and exit.
--restore FILE  Restore the given PostgreSQL dump
--database DB   Create the given database (default: $USER)
"'

exit_with_usage() {
    eval echo "$usage"
    exit "$1"
}

fail() {
    echo "$*" >&2
    exit_with_usage 1 >&2
}

RESTORE=""
DATABASE="$USER"

while case ${#} in (0) break ;; esac
do
    case ${1} in
    (--) shift; break ;;
    (-h|--help) exit_with_usage 0 ;;
    (--restore) shift; RESTORE=$1 ;;
    (--database|-d) shift; DATABASE=$1 ;;
    (-*) fail "unknown flag: ${1}" ;;
    (*) break ;;
    esac
    shift
done

export PGDATA=/tmp/pgsql/

if [[ -d "$PGDATA" ]]; then
    echo "'${PGDATA}' exists; delete directory before continuing" >&2
    exit 1
fi

mkdir -p "$PGDATA"
pg_ctl initdb
pg_ctl -l "${PGDATA}/postgres.log" -w start

createdb "$DATABASE"
echo "Created database: ${DATABASE}"

if [[ -n "$RESTORE" ]]; then
    case $(file "$RESTORE") in
        (*ASCII*) psql "$DATABASE" < "$RESTORE" ;;
        (*PostgreSQL*) pg_restore -d "$DATABASE" -1 "$RESTORE" --no-owner ;;
        (*) fail "unknown file type: ${RESTORE}" ;;
    esac
    echo "Restored dump: ${RESTORE}"
fi

echo ""
read -p "Press Enter to cleanup..."

pg_ctl stop
rm -rf "$PGDATA"
