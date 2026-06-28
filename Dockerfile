###########################################################
# Stage 1: Build the Haskell builder image with deps

FROM haskell:9.10.1-bullseye AS wrench-builder

RUN apt-get update && apt-get install -y \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY package.yaml stack.yaml stack.yaml.lock wrench.cabal /app/

RUN stack setup --install-ghc
RUN stack build --only-dependencies

###########################################################
# Stage 2.1: Build the Haskell application

FROM ryukzak/wrench-builder AS wrench-build
COPY . /app
ARG VERSION_SUFFIX=""
RUN echo "Building with VERSION_SUFFIX=${VERSION_SUFFIX}" && \
    stack build --ghc-options -O2 --copy-bins --local-bin-path /app/.local/bin

###########################################################
# Stage 2.2: Generate variants

FROM python:3.13-alpine3.21 AS wrench-variants

WORKDIR /app
COPY script /app

RUN [ "python", "/app/variants.py" ]

###########################################################
# Stage 2.3: Pre-render example reports

FROM debian:bullseye-slim AS wrench-examples

RUN apt-get update && apt-get install -y python3 libgmp10 libc6-dev ca-certificates locales \
    && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
    && locale-gen \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

WORKDIR /app
COPY --from=wrench-build /app/.local/bin/wrench /usr/local/bin/wrench
COPY example /app/example
COPY script/build_examples.py /app/build_examples.py

RUN python3 /app/build_examples.py \
    --wrench /usr/local/bin/wrench \
    --example-root /app/example \
    --output /app/out

###########################################################
# Stage 3: Create a minimal runtime container

FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y libgmp10 libc6-dev locales ca-certificates \
    && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
    && echo "ru_RU.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8
ENV VARIANTS=/app/variants
ENV EXAMPLES_PATH=/app/examples
ENV WRENCH_EXEC=wrench

WORKDIR /app
COPY --from=wrench-build /app/.local/bin/wrench /app/.local/bin/wrench-serv /app/.local/bin/wrench-fmt /bin/
COPY --from=wrench-variants /app/variants /app/variants
COPY --from=wrench-examples /app/out/storage /app/examples
COPY static /app/static

EXPOSE 8080

CMD ["wrench-serv"]
