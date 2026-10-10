#!/usr/bin/env bash
# Cut a release: bump the version, tag it and push.
#
# Publishing is CI's job -- pushing the tag triggers the build-release-image
# and publish-release workflows, which build ryukzak/wrench:<version> and
# :latest for all platforms with correct version metadata. Nothing is built or
# pushed to a registry from here.
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

if [ -n "$(git ls-remote --tags origin "refs/tags/$NEW_VERSION")" ]; then
    echo "Error: Git tag already exists on origin: $NEW_VERSION" >&2
    exit 1
fi

if curl -sfL -o /dev/null "https://hub.docker.com/v2/repositories/${IMAGE_NAME}/tags/${NEW_VERSION}"; then
    echo "Error: Image already published: ${IMAGE_NAME}:${NEW_VERSION}" >&2
    exit 1
fi

CURRENT_VERSION=$(grep '^version:' package.yaml | sed -E 's/version: //')
echo "Bumping version: $CURRENT_VERSION -> $NEW_VERSION"
sed -i.bak -E "s/^version: .*/version: $NEW_VERSION/" package.yaml
rm -f package.yaml.bak

# Regenerates wrench.cabal from package.yaml and proves the bump compiles.
make build

git add package.yaml wrench.cabal
git commit -m "chore: release $NEW_VERSION"
git tag -a "$NEW_VERSION" -m "Release $NEW_VERSION"
git push origin master
git push origin "$NEW_VERSION"

echo
echo "Tag $NEW_VERSION pushed. CI now builds and publishes:"
echo "  ${IMAGE_NAME}:${NEW_VERSION} and ${IMAGE_NAME}:latest (linux/amd64, linux/arm64)"
echo "Watch it: https://github.com/ryukzak/wrench/actions"
