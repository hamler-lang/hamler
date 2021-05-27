FROM erlang:23 as erlang

RUN apt update && apt install -y locales \
    && echo "LC_ALL=en_US.UTF-8" >> /etc/environment \
    && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && echo "LANG=en_US.UTF-8" > /etc/locale.conf \
    && locale-gen en_US.UTF-8

RUN apt update && \
    apt install -y libtinfo-dev build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 && \
    rm -rf /var/lib/apt/lists/*

ENV LC_ALL=en_US.UTF-8

ENV PATH=/root/.ghcup/bin:/root/.cabal/bin:/root/.local/bin:$PATH

RUN wget -nv --no-check-certificate https://get-ghcup.haskell.org -O ghcup.sh && \
    chmod +x ghcup.sh && \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 ./ghcup.sh

RUN cabal update && cabal install alex

RUN curl -sSL https://get.haskellstack.org/ | sh -s - -f && \
    stack install happy-1.19.9 --resolver lts-13.26 --allow-different-user

ENV PATH=/usr/lib/hamler/bin:$PATH

COPY . hamler
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
