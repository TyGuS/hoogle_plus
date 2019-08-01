# Pull base image.
FROM ubuntu:18.04

# install locales
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y locales

# Set the locale
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# install z3
RUN apt-get install -y git
RUN apt-get install -y build-essential
RUN apt-get install -y python
RUN cd /home; git clone https://github.com/Z3Prover/z3.git
RUN cd /home/z3; git checkout z3-4.8.1; python scripts/mk_make.py; cd build; make; make install

# install haskell stack tool
RUN apt-get install -y libtinfo-dev
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y haskell-stack
RUN stack upgrade
ENV PATH="/root/.local/bin:${PATH}"

# Start with bash
CMD ["/bin/bash"]

# To start the image, please mount the source file directory to /home/hoogle_plus
# docker run -v PATH_TO_HOOGLE_PLUS_SOURCE:/home/hoogle_plus -it hoogle_plus
# After the docker image is started
# run `cd /home/hoogle_plus; stack build`