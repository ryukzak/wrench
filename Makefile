.PHONY : test build format format-check lint clean

VERSION = $(shell cat package.yaml | grep version | sed -E 's/version: //')
COMMIT = $(shell git rev-parse --short HEAD)
IMAGE_NAME = ryukzak/wrench
IMAGE = $(IMAGE_NAME):$(VERSION)
IMAGE_PREVIEW = $(IMAGE_NAME):$(VERSION)-rc-$(COMMIT)

HS_SRC_DIR = .

build:
	stack build --copy-bins

server: build
	stack exec wrench-serv

build-image-local:
	docker build -t $(IMAGE_NAME) -f hub.Dockerfile .

preview-image:
	docker buildx build --platform linux/amd64,linux/arm64 -t $(IMAGE_NAME):preview -t $(IMAGE_PREVIEW) --push -f hub.Dockerfile .

release-image:
	@if docker pull $(IMAGE); then \
		echo "Version already exists: $(IMAGE)"; \
		exit 1; \
	fi
	docker buildx build --platform linux/amd64,linux/arm64 -t $(IMAGE_NAME) -t $(IMAGE) --push -f hub.Dockerfile .
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

format-fix:
	fourmolu -m inplace $(HS_SRC_DIR)
	ruff format script/*.py
	prettier -w static/
	yamlfmt example test

format-asm-fix: build
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

lint:
	hlint $(HS_SRC_DIR)
	ruff check script/*.py

clean:
	stack clean
	fd .result | xargs rm -v
	rm -v -R -f test/golden/generated/*
	rm -v -R -f variants/*
	rm -v -R -f variants.md
