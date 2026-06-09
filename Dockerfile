# syntax=docker/dockerfile:1.7

############################
# Build stage
############################
FROM haskell:9.6 AS build
WORKDIR /app

ARG TARGETPLATFORM
ARG EXE_NAME=integral-calculator

COPY *.cabal ./
COPY cabal.project* ./

RUN --mount=type=cache,target=/root/.cabal,id=cabal-${TARGETPLATFORM} \
    cabal update

RUN --mount=type=cache,target=/root/.cabal,id=cabal-${TARGETPLATFORM} \
    --mount=type=cache,target=/app/dist-newstyle,id=dist-${TARGETPLATFORM} \
    cabal build --only-dependencies -j

COPY . .

RUN --mount=type=cache,target=/root/.cabal,id=cabal-${TARGETPLATFORM} \
    --mount=type=cache,target=/app/dist-newstyle,id=dist-${TARGETPLATFORM} \
    cabal build -j

RUN --mount=type=cache,target=/app/dist-newstyle,id=dist-${TARGETPLATFORM} \
    set -eux; \
    mkdir -p /opt/bin; \
    BIN="$(find /app/dist-newstyle -type f -perm -111 -name "${EXE_NAME}" -print | head -n 1)"; \
    echo "Found binary: $BIN"; \
    test -n "$BIN"; \
    cp "$BIN" /opt/bin/app

############################
# Runtime stage
############################
FROM debian:bookworm-slim AS runtime

RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates \
      libgmp10 \
      libtinfo6 \
      zlib1g \
    && rm -rf /var/lib/apt/lists/*

ENV PORT=3000
EXPOSE 3000

COPY --from=build /opt/bin/app /usr/local/bin/integral-calculator

RUN useradd -m appuser
USER appuser

CMD ["/usr/local/bin/integral-calculator"]
