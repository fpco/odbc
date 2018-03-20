FROM ubuntu:16.04
MAINTAINER Chris Done

# Haskell system dependencies

RUN apt-get update && apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y librocksdb-dev  netbase git ca-certificates xz-utils build-essential curl && curl -sSL https://get.haskellstack.org/ | sh

# ODBC system dependencies

RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add
RUN apt-get install apt-transport-https && curl https://packages.microsoft.com/config/ubuntu/15.10/prod.list > /etc/apt/sources.list.d/mssql-release.list && apt-get update
RUN ACCEPT_EULA=Y apt-get install -y msodbcsql mssql-tools unixodbc-dev freetds-dev locales && locale-gen en_US.UTF-8

# Clone repo

RUN git clone https://github.com/fpco/odbc.git --branch bisect

# Install GHC and Haskell build dependencies

RUN cd odbc && stack setup && stack build --dependencies-only --test --no-run-tests
