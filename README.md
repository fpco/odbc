# odbc [![Build Status](https://travis-ci.org/fpco/odbc.svg)](https://travis-ci.org/fpco/odbc) [![Windows build status](https://ci.appveyor.com/api/projects/status/github/fpco/odbc?branch=master&svg=true)](https://ci.appveyor.com/project/fpco/odbc)

Haskell binding to the ODBC API, with a strong emphasis on stability,
testing and simplicity.

## Platform and database support

The following database drivers are tested against in CI:

* Microsoft SQL Server 2017

The following operating systems are tested against in CI:

* Windows [![Windows build status](https://ci.appveyor.com/api/projects/status/github/fpco/odbc?branch=master&svg=true)](https://ci.appveyor.com/project/fpco/odbc)
* Linux [![Build Status](https://travis-ci.org/fpco/odbc.svg)](https://travis-ci.org/fpco/odbc)

I develop and test this library on OS X, but currently do not have a
reliable way to run Microsoft SQL Server on Travis CI.

## How ODBC works

ODBC is a C API that is split into a *manager* and a *driver*.

On Windows, there is an ODBC manager that comes with the OS. On Linux
and OS X, the unixODBC package provides the same functionality.

Separately, for each database type, you have driver packages. When you
provide a connection string, like this:

```
ODBC_TEST_CONNECTION_STRING='DRIVER={ODBC Driver 13 for SQL Server};SERVER=127.0.0.1;Uid=SA;Pwd=Passw0rd;Encrypt=no'
```

The `DRIVER` tells the ODBC API which library to use. In this case,
it's the recent SQL Server driver provided by Microsoft. Then, ODBC
functions like `SQLDriverConnectW` will call that library.

## How to connect to Microsoft SQL Server

In recent years, Microsoft has released binary drivers for SQL Server
for Windows, Linux and OS X, with a guide for each operating
system. That guide for the latest and greatest official Microsoft
driver is
[here](https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server).

I have tested the OS X instructions on my own machine.
[This project's Dockerfile](https://github.com/fpco/odbc/blob/master/Dockerfile)
follows setup instructions for Linux, and
[the AppVeyor file](https://github.com/fpco/odbc/blob/master/appveyor.yml)
follows the setup instructions for Windows.

There is a test program that comes with the package called `odbc`
which accepts a connection string as its argument. You can use this to
test your connection easily.

    $ stack exec odbc 'DRIVER={ODBC Driver 13 for SQL Server};SERVER=192.168.99.101;Uid=SA;Pwd=Passw0rd;Encrypt=no'
    > create table foo (i int)
    Rows: 0
    > insert into foo values (123123123)
    Rows: 0
    > select * from foo
    123123123
    Rows: 1
