FROM debian:12.2

RUN apt-get update && apt-get upgrade -y && apt-get install -y make gnucobol

RUN groupadd --gid 10000 app \
    && useradd --uid 10000 --gid 10000 -m app

WORKDIR /home/app

USER 10000

COPY src src
COPY Makefile .

RUN make

CMD [ "./build/webserver" ]
