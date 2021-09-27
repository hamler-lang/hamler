FROM ghcr.io/hamler-lang/hamler/base:otp23.0-ghc8.10.4-cabal3.4.0.0-debian

COPY . hamler
ENV PATH=/usr/lib/hamler/bin:$PATH

RUN make -C hamler \
    && make -C hamler test \
    && make -C hamler install

WORKDIR /tmp/tests

RUN hamler init \
    && hamler build \
    && hamler run \
    && echo ":q" | hamler repl

WORKDIR /

CMD [ "hamler" ]
