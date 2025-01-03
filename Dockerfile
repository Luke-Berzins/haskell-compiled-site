FROM haskell:9.4

WORKDIR /site

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
    libgmp-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy only cabal file first for dependency caching
COPY compiled-website.cabal ./

# Install dependencies
RUN cabal update && \
    cabal build --only-dependencies

# Copy source code
COPY app ./app

CMD ["cabal", "run", "site", "watch"]