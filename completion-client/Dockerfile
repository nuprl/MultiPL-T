FROM lukemathwalker/cargo-chef:latest-rust-latest AS chef 

# Build the recipe 
WORKDIR /app
FROM chef as planner 
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

# Build the dependencies 
FROM chef as builder 
COPY --from=planner /app/recipe.json recipe.json
RUN cargo chef cook --release --recipe-path recipe.json

# Build the binary 
COPY . . 
RUN cargo build --release --bin client

FROM ubuntu:22.04 as runtime 

# Copied from MultiPL-E
RUN apt-get update -yqq && apt-get install -yqq \
    curl \
    build-essential \
    racket \
    lua5.3 luaunit \
    opam 

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
WORKDIR /client 

RUN opam init --disable-sandboxing -y
COPY --from=builder /app/target/release/client /usr/local/bin 


ENTRYPOINT [ "/usr/local/bin/client" ]



