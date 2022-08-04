#!/bin/bash

set -eu -o pipefail

SCRIPT=
VARS='{}'
TOKEN=

usage() {
    echo "Usage: ${0} [--vars VARS] [--token TOKEN] FILE [FILE ...]"
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        (--vars) shift; VARS="$1" ;;
        (--token) shift; TOKEN="$1" ;;
        (--help) usage; exit 0 ;;
        (*)
            SCRIPT="${SCRIPT}"$'\n'"$(cat "$1")"
        ;;
    esac
    shift
done

if [[ -z "${SCRIPT}" ]]; then
    usage; exit 1
fi

CURL_ARGS=(
    -H "Accept: application/vnd.github.antiope-preview+json"
    --data "{\"query\": $(jq -Rs <<< "${SCRIPT}"), \"variables\": ${VARS}}"
)

if [[ -n "${TOKEN}" ]]; then
    CURL_ARGS+=(-H "Authorization: Bearer ${TOKEN}")
fi

set -x
curl "${CURL_ARGS[@]}" https://api.github.com/graphql | jq .
