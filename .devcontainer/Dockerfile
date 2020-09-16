FROM mistzzt/haskell-language-server:0.4.0

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# Configure apt and install packages
RUN apt-get update && apt-get upgrade -y
RUN apt-get -y install --no-install-recommends apt-utils 2>&1 \
    #
    # Verify git, process tools, lsb-release (common in install instructions for CLIs) installed
    && apt-get -y install git procps lsb-release \
    && apt-get -y install curl wget screen build-essential \
    && apt-get -y install python make \
    #
    # Libraries required by GHC variant
    && apt-get -y install libtinfo5

# Install z3 Prover
RUN git clone --branch z3-4.8.1 https://github.com/Z3Prover/z3.git \
    && cd z3 \
    && python scripts/mk_make.py \
    && cd build \
    && make \
    && make install

# Clean z3 build files
RUN rm -rf /z3

# Get tools for the evaluation
RUN apt-get install -y python3 python3-pip
RUN pip3 install --user PyYAML numpy tabulate matplotlib argparse

RUN /usr/local/bin/stack config set system-ghc --global false
RUN /usr/local/bin/stack config set install-ghc --global true

# Clean up
RUN apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog
