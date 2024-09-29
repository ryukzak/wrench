FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y \
    libgmp10 libc6-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY dist/wrench dist/wrench-serv /bin/
COPY static /app/static

EXPOSE 8080

CMD ["wrench-serv"]
