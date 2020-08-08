# Pull base image.
FROM ubuntu:18.04
ARG port=3000
EXPOSE ${port}
EXPOSE 5000

# install locales
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y locales

# Set the locale
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# install z3
RUN apt-get install -y git build-essential python
RUN cd /home; git clone https://github.com/Z3Prover/z3.git
RUN cd /home/z3; git checkout z3-4.8.1; python scripts/mk_make.py; cd build; make; make install

# install haskell stack tool
RUN apt-get install -y libtinfo-dev zlib1g-dev haskell-stack
RUN stack upgrade
ENV PATH="/root/.local/bin:${PATH}"

# install curl
RUN apt-get install -y build-essential curl git

# install yarn
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN apt update && apt install -y yarn

# Get HooglePlus
RUN cd /home; git clone https://github.com/TyGuS/hoogle_plus.git
RUN cd /home/hoogle_plus && git checkout origin/oopsla20_artifact
RUN cd /home/hoogle_plus && stack build
RUN cd /home/hoogle_plus/new_webapp && yarn install

# Generate the database
RUN cd /home/hoogle_plus && stack exec -- hplus generate --preset=partialfunctions

# Get tools for the evaluation
RUN apt-get install -y python3 python3-pip
RUN pip3 install --user PyYAML numpy tabulate matplotlib argparse colorama pandas

# Start with bash
CMD /bin/bash