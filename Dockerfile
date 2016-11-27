FROM fpco/stack-build:lts-6.0

MAINTAINER Valentin Mahrwald <vmahrwald@gmail.com>

USER root

ADD src src
ADD test test
ADD static static
ADD stack.yaml .
ADD dominion.cabal .

RUN stack --no-docker build

EXPOSE 8000

CMD ["stack", "--no-docker", "exec", "dominion", "web"]
