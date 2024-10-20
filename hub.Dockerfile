###########################################################
# Stage 1: Build the Haskell application
FROM haskell:latest AS build

RUN apt-get update && apt-get install -y \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY package.yaml stack.yaml stack.yaml.lock wrench.cabal /app/

RUN stack setup --install-ghc
RUN stack build --only-dependencies

COPY . /app
RUN stack build --ghc-options -O2 --copy-bins --local-bin-path /app/.local/bin

###########################################################
# Stage 2: Create a minimal runtime container

FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y libgmp10 libc6-dev locales \
    && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
    && echo "ru_RU.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

WORKDIR /app
COPY --from=build /app/.local/bin/wrench /app/.local/bin/wrench-serv /bin/
COPY static /app/static

EXPOSE 8080

CMD ["wrench-serv"]
