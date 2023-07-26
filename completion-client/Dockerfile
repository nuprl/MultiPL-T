FROM docker.io/rust:1.71 as builder 

WORKDIR /usr/src/client
COPY . .
RUN cargo install --path .

FROM multipl-e-evaluation

RUN apt-get install -y libssl-dev

COPY --from=builder /usr/local/cargo/bin/completion-client /usr/local/bin/completion-client

ENTRYPOINT [ "/usr/local/bin/completion-client" ]
