FROM ubuntu:16.04
MAINTAINER Chris Done

# Setup

RUN apt-get update && apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y librocksdb-dev  netbase git ca-certificates xz-utils build-essential curl && curl -sSL https://get.haskellstack.org/ | sh

RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add

RUN apt-get install apt-transport-https && curl https://packages.microsoft.com/config/ubuntu/15.10/prod.list > /etc/apt/sources.list.d/mssql-release.list && apt-get update

RUN ACCEPT_EULA=Y apt-get install -y msodbcsql mssql-tools unixodbc-dev

# Clone repo

# Temporarily using this to access the repo
RUN curl https://chrisdone.com/private-toys/odbc.tar.gz > odbc.tar.gz && tar xzf odbc.tar.gz
# Once it's publicly released, change it to this:
#RUN git clone https://github.com/chrisdone/odbc.git --depth 1

# Install GHC

RUN cd odbc; stack setup

RUN cd odbc; stack build --dependencies-only --test --no-run-tests

# Build and run test suite

RUN apt-get install freetds-dev -y

RUN apt-get install -y locales && locale-gen en_US.UTF-8

RUN rm -r odbc; rm odbc.tar.gz; curl https://chrisdone.com/private-toys/odbc.tar.gz > odbc.tar.gz && tar xzf odbc.tar.gz

RUN cd odbc; stack test # --test-arguments "--seed 867408319"
