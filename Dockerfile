FROM ubuntu:20.04

USER root
ENV TERM linux

# set locale info
RUN apt-get update && apt-get install -y locales && locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV R_VERSION 4.2.2.20221110-1.2004.0

# set noninteractive installation
ENV DEBIAN_FRONTEND noninteractive

# see https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-18-04
# https://cran.r-project.org/bin/linux/debian/
# https://cran.r-project.org/bin/linux/ubuntu/README.html
RUN set -e \
      && apt-get update \
      && apt-get -y install --no-install-recommends --no-install-suggests \
        gnupg2 gnupg1 ca-certificates software-properties-common \
      && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
      && add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/' \
      && add-apt-repository ppa:git-core/ppa

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    # needed packages
    tzdata \
    sudo \
    less \
    build-essential \
    git-core \
    git-lfs \
    curl \
    pandoc \
    pandoc-citeproc \
    postgresql-client \
    libpq-dev \
    libssl-dev \
    openssl \
    libgdal-dev \
    libyaml-dev \
    libjpeg-dev \
    libxml2-dev \
    libxslt1-dev \
    libffi-dev \
    libblas-dev \
    liblapack-dev \
    libatlas-base-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libudunits2-dev \
    gfortran \
    unzip \
    zip \
    pbzip2 \
    vim-nox \
    nano \
    libbz2-dev \
    libsqlite3-dev \
    sqlite3 \
    openssh-server \
    libsnappy-dev \
    libncurses-dev \
    libreadline-dev \
    # add for debug
    libxt-dev \
    # for rstan
    libv8-dev \
    # for gdal
    gdal-bin \
    supervisor \
    awscli \
    r-base-core=$R_VERSION \
    postgresql \
    postgis \
    postgresql-12-postgis-3 \
    # make sure we have up-to-date CA certs or curling some https endpoints (like python.org) may fail
    ca-certificates \
    # app user creation
    && useradd -m app \
    && mkdir -p /home/app \
    && chown -R app:app /home/app \
    # set up sudo for app user
    && sudo echo "app ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/app \
    && sudo usermod -a -G staff app

WORKDIR /home/app
USER app
ENV HOME /home/app

####
# POSTGIS
####
# TODO: Set up postgis database
COPY --chown=app:app grant_cholera_database.sh $HOME/grant_cholera_database.sh
RUN sudo service postgresql start \
    && sudo -u postgres psql -c "CREATE DATABASE cholera_covariates;" \
    && sudo -u postgres psql -c "CREATE USER app WITH LOGIN;" \
    && sudo -u postgres psql -c "GRANT ALL ON DATABASE cholera_covariates TO app;" \
    && sudo -u postgres psql -d cholera_covariates -c "CREATE EXTENSION postgis;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION postgis_raster;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION postgis_topology;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION postgis_sfcgal;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION fuzzystrmatch;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION address_standardizer;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION address_standardizer_data_us;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION postgis_tiger_geocoder;" \
    && sudo -u app psql -d  cholera_covariates -c "CREATE SCHEMA covariates;" \
    && sudo -u app psql -d  cholera_covariates -c "CREATE SCHEMA data;" \
    && sudo -u app psql -d  cholera_covariates -c "CREATE SCHEMA grids;" \
    # && /bin/bash grant_cholera_database.sh app\
    && /bin/bash -c "/usr/bin/echo 'sudo service postgresql start' >> /home/app/.bashrc"

#####
# R
#####

RUN sudo Rscript -e "install.packages('renv',repos='https://cloud.r-project.org/')" \
    && cd /home/app 
    # && Rscript -e "renv::restore()"
    # && Rscript -e "cmdstanr::install_cmdstan()"
COPY --chown=app:app renv.cache $HOME/.cache
COPY --chown=app:app renv.lock $HOME/renv.lock
COPY --chown=app:app renv $HOME/renv
RUN  sudo Rscript -e "options(renv.config.install.verbose=TRUE);renv::restore(lockfile='$HOME/renv.lock', library='$HOME/renv/library/R-4.2/x86_64-pc-linux-gnu')"
COPY --chown=app:app Docker.Rprofile $HOME/.Rprofile

RUN git clone https://www.github.com/stan-dev/cmdstan --recurse-submodules \
  && cd cmdstan \
  && make build


# RUN git clone https://www.github.com/stan-dev/cmdstan --recurse-submodules \
#   && cd cmdstan \
#   && make build
# RUN /bin/bash -c "/usr/bin/echo 'sudo service postgresql start' >> /home/app/.bashrc"
RUN rm -rf $TMPDIR
CMD ["/bin/bash"]
