#!/usr/bin/env bash
# Guard against images whose `wrench --version` lost its build metadata.
#
# gitrev stamps the literal UNKNOWN whenever the build cannot read a git
# repository, which is silent at build time and only visible in a published
# image -- hence this check runs right after every push.
#
# Usage: check-image-version.sh <image> [version-suffix] [expected-version]
set -euo pipefail

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <image> [version-suffix] [expected-version]" >&2
    exit 1
fi

IMAGE="$1"
SUFFIX="${2-}"
EXPECTED_VERSION="${3-}"

ACTUAL=$(docker run --rm "$IMAGE" wrench --version)
echo "$IMAGE: $ACTUAL"

fail() {
    echo "Error: $1" >&2
    exit 1
}

case "$ACTUAL" in
    *UNKNOWN*)
        fail "version metadata is missing: built without GIT_HASH/GIT_DATE and without a usable .git"
        ;;
esac

if ! [[ "$ACTUAL" =~ ^[0-9]+(\.[0-9]+)*(-[A-Za-z0-9]+)?\ \([0-9a-f]{7}\)\ .+$ ]]; then
    fail "unexpected version string: '$ACTUAL'"
fi

if [ -n "$EXPECTED_VERSION" ]; then
    WANT="$EXPECTED_VERSION"
    if [ -n "$SUFFIX" ]; then
        WANT="$WANT-$SUFFIX"
    fi
    case "$ACTUAL" in
        "$WANT ("*) ;;
        *) fail "expected version '$WANT', got '$ACTUAL'" ;;
    esac
elif [ -n "$SUFFIX" ]; then
    case "$ACTUAL" in
        *"-$SUFFIX ("*) ;;
        *) fail "expected the '$SUFFIX' suffix, got '$ACTUAL'" ;;
    esac
fi

echo "$IMAGE: version metadata OK"
