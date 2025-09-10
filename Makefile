.PHONY : test build format format-check format-asm-check format-asm \
         format lint lint-fix clean build-image-local server-run \
         test-examples generate-variants update-golden fix markdown-fix \
         builder-image edge-image release-image format-python lint-python lint-fix-python

VERSION = $(shell cat package.yaml | grep version | sed -E 's/version: //')
COMMIT = $(shell git rev-parse --short HEAD)

BUILDER_IMAGE_NAME = ryukzak/wrench-builder

IMAGE_NAME = ryukzak/wrench

EDGE_IMAGE = $(IMAGE_NAME):edge
COMMIT_IMAGE = $(IMAGE_NAME):$(COMMIT)

RELEASE_IMAGE = $(IMAGE_NAME):$(VERSION)
LATEST_IMAGE = $(IMAGE_NAME):$(VERSION)

HS_SRC_DIR = .

export VERSION_SUFFIX ?= DEV

all: format lint-fix test test-serv

build:
	stack build --copy-bins

build-fmt:
	stack build --copy-bins :wrench-fmt

run-server: build generate-variants
	stack exec wrench-serv

build-image-local:
	docker build --build-arg VERSION_SUFFIX=DEV -t $(IMAGE_NAME) .

builder-image:
	docker buildx build --platform linux/amd64,linux/arm64 --push \
		-t $(BUILDER_IMAGE_NAME) --target wrench-builder .

edge-image:
	docker buildx build --build-arg VERSION_SUFFIX=EDGE --platform linux/amd64,linux/arm64 --push \
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
	docker buildx build --platform linux/amd64,linux/arm64 -t $(IMAGE_NAME) -t $(LATEST_IMAGE) --push .
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
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/huge-report.s  -c example/risc-iv-32/huge-report.yaml

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

	stack exec wrench -- --isa m68k       example/m68k/not.s                -c example/m68k/not-true.yaml
	stack exec wrench -- --isa m68k       example/m68k/get-put-char.s       -c example/m68k/get-put-char-87.yaml
	stack exec wrench -- --isa m68k       example/m68k/hello.s              -c example/m68k/hello.yaml
	stack exec wrench -- --isa m68k       example/m68k/hello-byte.s         -c example/m68k/hello.yaml
	stack exec wrench -- --isa m68k       example/m68k/factorial.s          -c example/m68k/factorial-5.yaml
	stack exec wrench -- --isa m68k       example/m68k/factorial-recursive.s -c example/m68k/factorial-5.yaml
	stack exec wrench -- --isa m68k       example/m68k/factorial-recursive-2.s -c example/m68k/factorial-5.yaml

test-serv: build generate-variants
	stack exec wrench-serv &
	hurl --retry 3 --no-output test/wrench-serv.hurl
	pkill -f wrench-serv

test-perf:
	stack build --ghc-options -O2 :wrench
	time stack exec wrench -- --isa f32a test/performance/program.s -c test/performance/conf.yaml

test-perf-prof:
	stack run --profile wrench -- +RTS -p -RTS --isa f32a test/performance/program.s -c test/performance/conf.yaml

generate-variants:
	script/variants.py

update-golden: generate-variants
	script/variants.py
	stack test --fast --test --test-arguments="--accept --rerun"

fix: lint-fix format update-golden markdown-fix
	stack ls dependencies | grep -v wrench > .stack-deps.txt

markdown-fix:
	markdownlint . .rules -c .markdownlint.yaml --fix

format: format-asm markdown-fix
	fourmolu -m inplace $(HS_SRC_DIR)
	ruff format script
	prettier -w static/
	yamlfmt package.yaml example test .github/workflows

format-asm: build-fmt
	stack exec wrench-fmt -- --inplace --isa risc-iv-32 -v example/risc-iv-32/*.s test/golden/risc-iv-32/*.s
	stack exec wrench-fmt -- --inplace --isa f32a       -v example/f32a/*.s       test/golden/f32a/*.s
	stack exec wrench-fmt -- --inplace --isa acc32      -v example/acc32/*.s      test/golden/acc32/*.s
	stack exec wrench-fmt -- --inplace --isa m68k       -v example/m68k/*.s       test/golden/m68k/*.s

format-asm-check: build-fmt
	stack exec wrench-fmt -- --check   --isa risc-iv-32 -v example/risc-iv-32/*.s test/golden/risc-iv-32/*.s
	stack exec wrench-fmt -- --check   --isa f32a       -v example/f32a/*.s       test/golden/f32a/*.s
	stack exec wrench-fmt -- --check   --isa acc32      -v example/acc32/*.s      test/golden/acc32/*.s
	stack exec wrench-fmt -- --check   --isa m68k       -v example/m68k/*.s       test/golden/m68k/*.s

format-check:
	fourmolu -m check $(HS_SRC_DIR)

lint-fix:
	fd -tf .hs | xargs -n 1 -P 8 hlint --refactor --refactor-options="--inplace"
	ruff check script --fix

lint:
	hlint $(HS_SRC_DIR)
	ruff check script

format-python:
	ruff format script

lint-python:
	ruff check script

lint-fix-python:
	ruff check script --fix

clean:
	stack clean
	fd .result | xargs rm -v
	rm -v -R -f test/golden/generated/*
	rm -v -R -f variants/*
	rm -v -R -f variants.md
