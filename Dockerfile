FROM debian:9-slim
MAINTAINER Chris Done

# Haskell system dependencies

RUN apt-get update && apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y netbase git ca-certificates xz-utils build-essential curl && curl -sSL https://get.haskellstack.org/ | sh

# ODBC system dependencies

RUN apt-get install -y gnupg apt-transport-https
RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
RUN curl https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list
RUN apt-get update
RUN ACCEPT_EULA=Y apt-get install msodbcsql17 -y
RUN apt-get install -y unixodbc-dev freetds-dev locales
RUN locale-gen en_US.UTF-8

# Install GHC and Haskell build dependencies

RUN stack setup && stack build --dependencies-only --test --no-run-tests
