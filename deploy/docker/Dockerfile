FROM erlang:22.3

RUN apt update && apt install -y locales \
    && echo "LC_ALL=en_US.UTF-8" >> /etc/environment \
    && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && echo "LANG=en_US.UTF-8" > /etc/locale.conf \
    && locale-gen en_US.UTF-8

ENV LC_ALL=en_US.UTF-8

RUN apt update && apt install -y libtinfo-dev ghc

RUN wget -qO- https://get.haskellstack.org/ | sh

ENV PATH=/root/.local/bin:$PATH

COPY . hamler
RUN sed -i -r 's:^path (.*):path /usr/lib/hamler:g' hamler/Env \
    && make -C hamler \
    && make -C hamler test \
    && make -C hamler install

WORKDIR /tmp/tests

RUN hamler init \ 
    && hamler build \
    && hamler run \
    && echo ":q" | hamler repl

WORKDIR /

CMD [ "hamler" ]