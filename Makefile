.PHONY : test build format format-check lint clean

VERSION = $(shell cat package.yaml | grep version | sed -E 's/version: //')
COMMIT = $(shell git rev-parse --short HEAD)

BUILDER_IMAGE_NAME = ryukzak/wrench-builder

IMAGE_NAME = ryukzak/wrench

EDGE_IMAGE = $(IMAGE_NAME):edge
COMMIT_IMAGE = $(IMAGE_NAME):$(COMMIT)

RELEASE_IMAGE = $(IMAGE_NAME):$(VERSION)
LATEST_IMAGE = $(IMAGE_NAME):$(VERSION)

HS_SRC_DIR = .

build:
	stack build --copy-bins

server-run: build
	stack exec wrench-serv

build-image-local:
	docker build -t $(IMAGE_NAME) .

builder-image:
	docker buildx build --platform linux/amd64,linux/arm64 --push \
		-t $(BUILDER_IMAGE_NAME) --target wrench-builder .

edge-image:
	docker buildx build --platform linux/amd64,linux/arm64 --push \
		-t $(EDGE_IMAGE) -t $(COMMIT_IMAGE) .

release-image:
	@if [ "$(shell git symbolic-ref --short HEAD)" != "master" ]; then \
		echo "Error: You must be on the master branch to release."; \
		exit 1; \
	fi
	@if ! git diff-index --quiet HEAD --; then \
		echo "Error: You have uncommitted changes. Please commit or stash them before releasing."; \
		exit 1; \
	fi
	@if [ "$(shell git rev-list --count --left-only HEAD...@{u})" -gt 0 ]; then \
		echo "Error: You have unpushed commits. Please push them before releasing."; \
		exit 1; \
	fi
	git fetch
	@if [ "$(shell git rev-list --count --left-only @{u}...HEAD)" -gt 0 ]; then \
		echo "Error: You have unpulled commits. Please pull them before releasing."; \
		exit 1; \
	fi
	@if docker pull $(IMAGE); then \
		echo "Version already exists: $(IMAGE)"; \
		exit 1; \
	fi
	docker buildx build --platform linux/amd64,linux/arm64 -t $(IMAGE_NAME) -t $(IMAGE) --push -f hub.Dockerfile .
	# docker pull $(COMMIT_IMAGE)
	# docker image tag $(COMMIT_IMAGE) $(RELEASE_IMAGE)
	# docker image tag $(COMMIT_IMAGE) $(LATEST_IMAGE)
	git tag -a $(VERSION) -m "Release $(VERSION)"
	git push origin $(VERSION)

test:
	stack build --fast --test --test-arguments "--rerun"

test-examples: build
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/factorial.s    -c example/risc-iv-32/factorial-5.yaml
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/hello.s        -c example/risc-iv-32/hello.yaml
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/get-put-char.s -c example/risc-iv-32/get-put-char-87.yaml
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/get-put-char.s -c example/risc-iv-32/get-put-char-ABCD.yaml
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/not.s          -c example/risc-iv-32/not-true.yaml

	stack exec wrench -- --isa f32a       example/f32a/not.s                -c example/f32a/not-true.yaml
	stack exec wrench -- --isa f32a       example/f32a/hello.s              -c example/f32a/hello.yaml
	# stack exec wrench -- --isa f32a       example/f32a/get-put-char.s       -c example/f32a/get-put-char-87.yaml
	# stack exec wrench -- --isa f32a       example/f32a/get-put-char.s       -c example/f32a/get-put-char-ABCD.yaml
	stack exec wrench -- --isa f32a       example/f32a/factorial.s          -c example/f32a/factorial.yaml

	stack exec wrench -- --isa acc32      example/acc32/not.s               -c example/acc32/not-true.yaml
	stack exec wrench -- --isa acc32      example/acc32/hello.s             -c example/acc32/hello.yaml
	stack exec wrench -- --isa acc32      example/acc32/get-put-char.s      -c example/acc32/get-put-char-87.yaml
	stack exec wrench -- --isa acc32      example/acc32/get-put-char.s      -c example/acc32/get-put-char-ABCD.yaml
	stack exec wrench -- --isa acc32      example/acc32/factorial.s         -c example/acc32/factorial-5.yaml

update-golden:
	script/variants.py
	stack test --fast --test --test-arguments="--accept --rerun"
fix: lint-fix format-fix update-golden readme-fix

readme-fix:
	markdownlint . -c .markdownlint.yaml --fix

format-fix: format-asm-fix
	fourmolu -m inplace $(HS_SRC_DIR)
	ruff format script
	prettier -w static/
	yamlfmt example test

format-asm-fix:
	stack exec wrench-fmt -- --inplace --isa risc-iv-32 -v example/risc-iv-32/*.s test/golden/risc-iv-32/*.s
	stack exec wrench-fmt -- --inplace --isa f32a -v example/f32a/*.s test/golden/f32a/*.s
	stack exec wrench-fmt -- --inplace --isa acc32 -v example/acc32/*.s test/golden/acc32/*.s

format-asm-check: build
	stack exec wrench-fmt -- --check --isa risc-iv-32 -v example/risc-iv-32/*.s test/golden/risc-iv-32/*.s
	stack exec wrench-fmt -- --check --isa f32a -v example/f32a/*.s test/golden/f32a/*.s
	stack exec wrench-fmt -- --check --isa acc32 -v example/acc32/*.s test/golden/acc32/*.s

format-check:
	fourmolu -m check $(HS_SRC_DIR)

lint-fix:
	fd .hs | xargs -n 1 -P 8 hlint --refactor --refactor-options="--inplace"
	ruff check script --fix

lint:
	hlint $(HS_SRC_DIR)
	ruff check script

clean:
	stack clean
	fd .result | xargs rm -v
	rm -v -R -f test/golden/generated/*
	rm -v -R -f variants/*
	rm -v -R -f variants.md
