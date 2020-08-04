FROM mistzzt/haskell-ide-engine:1.4-ghc8.4.4

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# Configure apt and install packages
RUN apt-get update && apt-get upgrade -y
RUN apt-get -y install --no-install-recommends apt-utils 2>&1 \
    #
    # Verify git, process tools, lsb-release (common in install instructions for CLIs) installed
    && apt-get -y install git procps lsb-release \
    && apt-get -y install curl wget screen build-essential \
    && apt-get -y install python make 

# Install z3 Prover
RUN git clone --branch z3-4.7.1 https://github.com/Z3Prover/z3.git \
    && cd z3 \
    && python scripts/mk_make.py \
    && cd build \
    && make \
    && make install

# Clean z3 build files
RUN rm -rf /z3

# Clean up
RUN apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog
