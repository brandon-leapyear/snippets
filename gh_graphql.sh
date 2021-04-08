#!/bin/bash

set -u -o pipefail

SCRIPT=
VARS='{}'
TOKEN=

usage() {
    echo "Usage: ${0} [--vars VARS] [--token TOKEN] FILE"
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        (--vars) shift; VARS="$1" ;;
        (--token) shift; TOKEN="$1" ;;
        (--help) usage; exit 0 ;;
        (*)
            if [[ -z "${SCRIPT}" ]]; then
                SCRIPT="$(cat "$1" | jq -Rs)"
            else
                usage; exit 1
            fi
        ;;
    esac
    shift
done

if [[ -z "${SCRIPT}" ]]; then
    usage; exit 1
fi

CURL_ARGS=(
    -H "Accept: application/vnd.github.antiope-preview+json"
    --data "{\"query\": ${SCRIPT}, \"variables\": ${VARS}}"
)

if [[ -n "${TOKEN}" ]]; then
    CURL_ARGS+=(-H "Authorization: Bearer ${TOKEN}")
fi

curl "${CURL_ARGS[@]}" https://api.github.com/graphql
