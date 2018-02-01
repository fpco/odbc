# odbc [![Build Status](https://travis-ci.org/chrisdone/odbc.svg)](https://travis-ci.org/chrisdone/odbc) [![Windows build status](https://ci.appveyor.com/api/projects/status/github/chrisdone/odbc?branch=master&svg=true)](https://ci.appveyor.com/project/chrisdone/odbc)

Haskell binding to the ODBC API, with a strong emphasis on stability,
testing and simplicity.

## Platform and database support

The following database drivers are tested against in CI:

* Microsoft SQL Server 2017

The following operating systems are tested against in CI:

* Windows [![Windows build status](https://ci.appveyor.com/api/projects/status/github/chrisdone/odbc?branch=master&svg=true)](https://ci.appveyor.com/project/chrisdone/odbc)
* Linux [![Build Status](https://travis-ci.org/chrisdone/odbc.svg)](https://travis-ci.org/chrisdone/odbc)

I develop and test this library on OS X, but currently do not have a
reliable way to run Microsoft SQL Server on Travis CI.
