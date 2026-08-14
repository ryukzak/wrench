.PHONY: build build-dist build-examples build-fmt \
        test test-hs test-examples test-server test-accept test-perf test-perf-profile \
        format format-hs format-asm format-py format-md format-html format-yaml \
        format-check format-check-hs format-check-asm format-check-py format-check-html format-check-md \
        lint lint-hs lint-py lint-html \
        lint-fix lint-fix-hs lint-fix-py \
        generate generate-variants generate-stack-deps \
        run-server \
        docker-build docker-push-builder docker-push-edge \
        fix clean

COMMIT = $(shell git rev-parse --short HEAD)

BUILDER_IMAGE_NAME = ryukzak/wrench-builder

IMAGE_NAME = ryukzak/wrench

EDGE_IMAGE = $(IMAGE_NAME):edge
COMMIT_IMAGE = $(IMAGE_NAME):$(COMMIT)

HS_SRC_DIR = .

export VERSION_SUFFIX ?= DEV

all: format lint-fix test

# Build

build:
	stack build --copy-bins

build-fmt:
	stack build --copy-bins :wrench-fmt

build-examples:
	python script/build_examples.py --fail-fast

# Run

run-server: build generate-variants
	stack exec wrench-serv

# Test

test: test-hs test-examples test-server

test-hs:
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
	stack exec wrench -- --isa m68k       example/m68k/hello-byte.s         -c example/m68k/hello-byte.yaml
	stack exec wrench -- --isa m68k       example/m68k/factorial.s          -c example/m68k/factorial-5.yaml
	stack exec wrench -- --isa m68k       example/m68k/factorial-recursive.s -c example/m68k/factorial-recursive.yaml
	stack exec wrench -- --isa m68k       example/m68k/factorial-recursive-2.s -c example/m68k/factorial-recursive-2.yaml

	stack exec wrench -- --isa vliw-iv    example/vliw-iv/hello.s           -c example/vliw-iv/hello.yaml
	stack exec wrench -- --isa vliw-iv    example/vliw-iv/factorial.s       -c example/vliw-iv/factorial-5.yaml
	stack exec wrench -- --isa vliw-iv    example/vliw-iv/test-parallel.s   -c example/vliw-iv/test-parallel.yaml

test-server: build generate-variants
	stack exec wrench-serv &
	hurl --retry 3 --no-output test/wrench-serv.hurl
	pkill -f wrench-serv

test-perf:
	stack build --ghc-options -O2 :wrench
	time stack exec wrench -- --isa f32a test/performance/program.s -c test/performance/conf.yaml

test-perf-profile:
	stack run --profile wrench -- +RTS -p -RTS --isa f32a test/performance/program.s -c test/performance/conf.yaml

test-accept: generate-variants
	stack test --fast --test --test-arguments="--accept --rerun"

# Format – apply

format: format-hs format-asm format-py format-md format-html format-yaml

format-hs:
	fourmolu -m inplace $(HS_SRC_DIR)

format-asm: build-fmt
	stack exec wrench-fmt -- --inplace --isa risc-iv-32 -v example/risc-iv-32/*.s test/golden/risc-iv-32/*.s
	stack exec wrench-fmt -- --inplace --isa f32a       -v example/f32a/*.s       test/golden/f32a/*.s
	stack exec wrench-fmt -- --inplace --isa acc32      -v example/acc32/*.s      test/golden/acc32/*.s
	stack exec wrench-fmt -- --inplace --isa m68k       -v example/m68k/*.s       test/golden/m68k/*.s
	stack exec wrench-fmt -- --inplace --isa vliw-iv    -v example/vliw-iv/*.s    test/golden/vliw-iv/*.s

format-py:
	ruff format script

format-md:
	markdownlint . .rules -c .markdownlint.yaml --fix

format-html:
	npx @biomejs/biome format --write static/

format-yaml:
	yamlfmt package.yaml example test .github/workflows

# Format – check

format-check: format-check-hs format-check-asm format-check-py format-check-html format-check-md

format-check-hs:
	fourmolu -m check $(HS_SRC_DIR)

format-check-asm: build-fmt
	stack exec wrench-fmt -- --check --isa risc-iv-32 -v example/risc-iv-32/*.s test/golden/risc-iv-32/*.s
	stack exec wrench-fmt -- --check --isa f32a       -v example/f32a/*.s       test/golden/f32a/*.s
	stack exec wrench-fmt -- --check --isa acc32      -v example/acc32/*.s      test/golden/acc32/*.s
	stack exec wrench-fmt -- --check --isa m68k       -v example/m68k/*.s       test/golden/m68k/*.s
	stack exec wrench-fmt -- --check --isa vliw-iv    -v example/vliw-iv/*.s    test/golden/vliw-iv/*.s

format-check-py:
	ruff format --check script

format-check-html:
	npx @biomejs/biome format static/

format-check-md:
	markdownlint . .rules -c .markdownlint.yaml

# Lint – check

lint: lint-hs lint-py lint-html

lint-hs:
	hlint $(HS_SRC_DIR)

lint-py:
	ruff check script

lint-html:
	npx @biomejs/biome lint static/

# Lint – fix

lint-fix: lint-fix-hs lint-fix-py

lint-fix-hs:
	fd -tf .hs | xargs -n 1 -P 8 hlint --refactor --refactor-options="--inplace"

lint-fix-py:
	ruff check script --fix

# Generate

generate: generate-variants generate-stack-deps

generate-variants:
	mkdir -p variants
	script/variants.py

generate-stack-deps:
	stack ls dependencies | grep -v wrench > .stack-deps.txt

# Docker

docker-build:
	docker build --build-arg VERSION_SUFFIX=DEV -t $(IMAGE_NAME) .

docker-push-builder: generate-stack-deps
	docker buildx build --platform linux/amd64,linux/arm64 --push \
		-t $(BUILDER_IMAGE_NAME) --target wrench-builder .

docker-push-edge:
	docker buildx build --build-arg VERSION_SUFFIX=EDGE --platform linux/amd64,linux/arm64 --push \
		-t $(EDGE_IMAGE) -t $(COMMIT_IMAGE) .

# Meta

fix: lint-fix format generate test-accept test-examples test-server

clean:
	stack clean
	fd .result | xargs rm -v
	rm -v -R -f test/golden/generated/*
	rm -v -R -f variants/*
	rm -v -R -f variants.md
