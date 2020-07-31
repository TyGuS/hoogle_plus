# Pull base image.
FROM ubuntu:18.04
ARG port=5000
EXPOSE ${port}

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

# Get tools for the evaluation
RUN apt-get install -y python3 python3-pip
RUN pip3 install --user PyYAML numpy tabulate matplotlib argparse

# Get HooglePlus
RUN cd /home; git clone https://github.com/TyGuS/hoogle_plus.git
RUN cd /home/hoogle_plus && git checkout origin/new_webapp_deploy
RUN cd /home/hoogle_plus && stack build

# Start with bash
RUN cd /home/hoogle_plus && stack exec -- hplus generate --preset=partialfunctions

# CMD cd /home/hoogle_plus && stack run webapp -p ${port} >> /var/log/hplus/run.log
CMD cd /home/hoogle_plus/web_server && pip3 install -r dependencies.txt && ./start_server.sh

# HEALTHCHECK CMD curl --fail http://localhost:${port}/ || exit 1

# To start the image, please mount the source file directory to /home/hoogle_plus
# docker run -v PATH_TO_HOOGLE_PLUS_SOURCE:/home/hoogle_plus -it hoogle_plus
# After the docker image is started
# run `cd /home/hoogle_plus; stack build`

#
# REMEMBER TO TAG CHANGES WITH LATEST.
# LATEST TAG IS NOT UPDATED AUTOMATICALLY
#
