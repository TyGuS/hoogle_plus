# Pull base image.
FROM ubuntu:18.04
ARG port=3000
EXPOSE ${port}

ARG backend

# install locales
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y locales

# Set the locale
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# install curl
RUN apt-get install -y build-essential curl git

# install yarn
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN apt update && apt install -y yarn

# get repo and run
RUN cd /home; git clone https://github.com/TyGus/hoogle_plus.git
RUN cd /home/hoogle_plus && git checkout origin/new_webapp_deploy
RUN cd /home/hoogle_plus/new_webapp && yarn install

CMD cd /home/hoogle_plus/new_webapp && REACT_APP_DEVELOPMENT_HOST=${backend} yarn start
