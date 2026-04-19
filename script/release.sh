#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
    CURRENT_VERSION=$(grep '^version:' package.yaml | sed -E 's/version: //')
    echo "Usage: $0 <new-version>" >&2
    echo "Example: $0 0.0.4.2" >&2
    echo "Current version: $CURRENT_VERSION" >&2
    exit 1
fi

NEW_VERSION="$1"

if ! [[ "$NEW_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+(\.[0-9]+)?$ ]]; then
    echo "Error: Invalid version format '$NEW_VERSION'. Expected: X.Y.Z or X.Y.Z.W" >&2
    exit 1
fi

IMAGE_NAME="ryukzak/wrench"
RELEASE_IMAGE="${IMAGE_NAME}:${NEW_VERSION}"

if [ "$(git symbolic-ref --short HEAD)" != "master" ]; then
    echo "Error: You must be on the master branch to release." >&2
    exit 1
fi

if ! git diff-index --quiet HEAD --; then
    echo "Error: You have uncommitted changes. Please commit or stash them before releasing." >&2
    exit 1
fi

if [ "$(git rev-list --count --left-only HEAD...@{u})" -gt 0 ]; then
    echo "Error: You have unpushed commits. Please push them before releasing." >&2
    exit 1
fi

git fetch

if [ "$(git rev-list --count --left-only @{u}...HEAD)" -gt 0 ]; then
    echo "Error: You have unpulled commits. Please pull them before releasing." >&2
    exit 1
fi

if git rev-parse -q --verify "refs/tags/$NEW_VERSION" >/dev/null; then
    echo "Error: Git tag already exists: $NEW_VERSION" >&2
    exit 1
fi

if docker pull "$RELEASE_IMAGE" >/dev/null 2>&1; then
    echo "Error: Version already exists: $RELEASE_IMAGE" >&2
    exit 1
fi

CURRENT_VERSION=$(grep '^version:' package.yaml | sed -E 's/version: //')
echo "Bumping version: $CURRENT_VERSION -> $NEW_VERSION"
sed -i.bak -E "s/^version: .*/version: $NEW_VERSION/" package.yaml
rm -f package.yaml.bak

docker buildx build --platform linux/amd64,linux/arm64 \
    -t "$IMAGE_NAME" -t "$RELEASE_IMAGE" --push .

git add package.yaml
git commit -m "chore: release $NEW_VERSION"
git tag -a "$NEW_VERSION" -m "Release $NEW_VERSION"
git push origin master
git push origin "$NEW_VERSION"

echo "Release $NEW_VERSION completed successfully."
