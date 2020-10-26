FROM ubuntu:20.04

USER root
ENV TERM linux

# set locale info
RUN apt-get update && apt-get install -y locales && locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# set noninteractive installation
ENV DEBIAN_FRONTEND noninteractive
ENV R_VERSION 4.0.3-1.2004.0

# see https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-18-04
# https://cran.r-project.org/bin/linux/debian/
# https://cran.r-project.org/bin/linux/ubuntu/README.html
RUN set -e \
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
    supervisor \
    awscli \
    r-base-dev=$R_VERSION \
    postgresql \
    postgis \
    postgresql-12-postgis-3 \
    # make sure we have up-to-date CA certs or curling some https endpoints (like python.org) may fail
    ca-certificates \
    # app user creation
    && useradd -m app \
    && mkdir -p /home/app \
    && chown app:app /home/app \
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
RUN sudo service postgresql start \
    && sudo -u postgres psql -c "CREATE DATABASE cholera_covariates;" \
    && sudo -u postgres psql -c "CREATE USER app WITH LOGIN;" \
    && sudo -u postgres psql -c "GRANT CONNECT ON DATABASE cholera_covariates TO app;" \
    && sudo -u postgres psql -d cholera_covariates -c "CREATE EXTENSION postgis;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION postgis_raster;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION postgis_topology;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION postgis_sfcgal;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION fuzzystrmatch;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION address_standardizer;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION address_standardizer_data_us;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE EXTENSION postgis_tiger_geocoder;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE SCHEMA covariates;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE SCHEMA data;" \
    && sudo -u postgres psql -d  cholera_covariates -c "CREATE SCHEMA grids;" \
    && /bin/bash -c "/usr/bin/echo 'sudo service postgresql start' >> /home/app/.bashrc"

#####
# R
#####

RUN Rscript -e "install.packages('packrat',repos='https://cloud.r-project.org/')"
COPY --chown=app:app packrat $HOME/packrat
COPY --chown=app:app Docker.Rprofile $HOME/.Rprofile
COPY --chown=app:app packages $HOME/R/pkgs
RUN Rscript -e 'packrat::restore()'


# RUN /bin/bash -c "/usr/bin/echo 'sudo service postgresql start' >> /home/app/.bashrc"

CMD ["/bin/bash"]
