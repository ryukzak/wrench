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

RUN apt-get update && apt-get install -y \
    libgmp10 libc6-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /app/.local/bin/wrench /app/.local/bin/wrench-serv /bin/
COPY static /app/static

EXPOSE 8080

CMD ["wrench-serv"]
