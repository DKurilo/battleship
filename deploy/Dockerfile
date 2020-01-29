FROM ubuntu:18.04
MAINTAINER Dima Kurilo <dkurilo@gmail.com>

RUN apt-get update \
&& apt-get install -y git zlib1g-dev libnuma-dev libssl-dev \
curl gnupg gnupg1 gnupg2 xz-utils gcc clang make libgmp-dev libgmp10

RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list

RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -

RUN apt-get update \
&& apt-get install -y nodejs yarn\
&& rm -rf /var/lib/apt/lists/*

RUN curl -sSL https://get.haskellstack.org/ | sh

VOLUME /var/build
WORKDIR /var/build

CMD ["/var/build/deploy/build.sh"]
