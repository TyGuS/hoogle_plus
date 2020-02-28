# Pull base image.
FROM ubuntu:18.04
EXPOSE 3000/tcp

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
RUN cd /home/z3; git checkout z3-4.7.1; python scripts/mk_make.py; cd build; make -j 6 && make install
ENV LD_LIBRARY_PATH="/home/z3/build/"

# install haskell stack tool
RUN apt-get install -y libtinfo-dev
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y haskell-stack
RUN stack upgrade
ENV PATH="/root/.local/bin:${PATH}"

# Get tools for the evaluation
RUN apt-get install -y python3 python3-pip
RUN pip3 install --user PyYAML numpy tabulate matplotlib argparse

# Get HooglePlus
RUN cd /home; git clone https://github.com/davidmrdavid/hoogle_plus.git
RUN cd /home/hoogle_plus && git fetch -a
# RUN cd /home/hoogle_plus && git checkout remotes/origin/topDown_MarDarDan
RUN cd /home/hoogle_plys && git checkout --track origin/topDown_MarDarDan
RUN cd /home/hoogle_plus && stack build

# Start with bash
RUN cd /home/hoogle_plus && stack exec -- hplus generate --preset=popl2020

# Re-run the evaluation
# RUN cd /home/hoogle_plus && \
#     ./scripts/run_each_benchmark.py && \
#     ./scripts/collect_tsv.py && \
#     pdflatex table.tex

# CMD cd /home/hoogle_plus && /bin/bash

# HEALTHCHECK CMD curl --fail http://localhost:3000/ || exit 1

# To start the image, please mount the source file directory to /home/hoogle_plus
# docker run -v PATH_TO_HOOGLE_PLUS_SOURCE:/home/hoogle_plus -it hoogle_plus
# After the docker image is started
# run `cd /home/hoogle_plus; stack build`

#
# REMEMBER TO TAG CHANGES WITH LATEST.
# LATEST TAG IS NOT UPDATED AUTOMATICALLY
#
