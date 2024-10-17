.PHONY : test run build format format-check lint clean

HS_SRC_DIR = .

run:
	stack build --fast && stack exec ca-wrench-exe

build:
	stack build --copy-bins

build-image:
	docker build -t ryukzak/wrench -f hub.Dockerfile .

build-image-for-hub:
	docker build -t ryukzak/wrench --push -f hub.Dockerfile .

test:
	stack build --fast --test

test-examples: build
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/factorial.s    -c example/risc-iv-32/factorial-5.yaml
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/hello.s        -c example/risc-iv-32/hello.yaml
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/get-put-char.s -c example/risc-iv-32/get-put-char-87.yaml
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/get-put-char.s -c example/risc-iv-32/get-put-char-ABCD.yaml
	stack exec wrench -- --isa risc-iv-32 example/risc-iv-32/not.s          -c example/risc-iv-32/not-true.yaml
	stack exec wrench -- --isa f18a       example/f18a/not.s                -c example/f18a/not-true.yaml
	stack exec wrench -- --isa f18a       example/f18a/hello.s              -c example/f18a/hello.yaml

update-golden:
	stack test --fast --test --test-arguments=--accept

fix: lint-fix format-fix update-golden readme-fix

readme-fix:
	markdownlint *.md -c .markdownlint.yaml --fix

format-fix:
	fourmolu -m inplace $(HS_SRC_DIR)

format-check:
	fourmolu -m check $(HS_SRC_DIR)

lint-fix:
	fd .hs | xargs -n 1 -P 8 hlint --refactor  --refactor-options="--inplace"

lint:
	hlint $(HS_SRC_DIR)

clean:
	stack clean
	fd .result | xargs rm -v
