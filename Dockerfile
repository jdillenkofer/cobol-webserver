FROM debian:12.1

RUN apt-get update && apt-get upgrade && apt-get install -y make gnucobol

RUN groupadd --gid 10000 app \
    && useradd --uid 10000 --gid 10000 -m app

WORKDIR /home/app

USER 10000

COPY webserver.cob .
COPY Makefile .

RUN make

CMD [ "./webserver" ]
