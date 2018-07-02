# odbc [![Build Status](https://travis-ci.org/fpco/odbc.svg)](https://travis-ci.org/fpco/odbc) [![Build status](https://ci.appveyor.com/api/projects/status/vpn6a1pme25upbux?svg=true)](https://ci.appveyor.com/project/chrisdone/odbc-0os0b)

Haskell binding to the ODBC API, with a strong emphasis on stability,
testing and simplicity.

## Platform and database support

The following database drivers are tested against in CI:

* Microsoft SQL Server 2017

The following operating systems are tested against in CI:

* Windows [![Build status](https://ci.appveyor.com/api/projects/status/vpn6a1pme25upbux?svg=true)](https://ci.appveyor.com/project/chrisdone/odbc-0os0b)
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

(Use `17` instead of `13` if that's the driver you installed.)

    $ stack exec odbc 'DRIVER={ODBC Driver 13 for SQL Server};SERVER=192.168.99.101;Uid=SA;Pwd=Passw0rd;Encrypt=no'
    > create table foo (i int)
    Rows: 0
    > insert into foo values (123123123)
    Rows: 0
    > select * from foo
    123123123
    Rows: 1

## Common issues

Compilation on Linux/OS X may require a `odbcss.h` header file for type/constant definitions. To get this install the freetds package:

* [Linux example](https://github.com/fpco/odbc/blob/efe81f7c17f5ff4c1cf8937577b32f049c0dd62b/Dockerfile#L15).
* On OS X you can use `brew install freetds`.

Windows should already have this file.

If you see an error like this:

    [unixODBC][Driver Manager]Can't open lib 'ODBC Driver 13 for SQL Server' : file not found

Then you might be trying to use the wrong driver. You might have
installed version `17`, so change the string to `ODBC Driver 17 for
SQL Server`.

If you see an error like this:

    [unixODBC][Driver Manager]Data source name not found and no default driver specified

This is a terrible error message. If passing your DSN via a shell
environment variable or argument, check that your input string isn't
quoted e.g. `"Driver=.."` instead of `Driver=..` due to silly shell
scripting quoting issues.

If you see an error like this on OS X with driver version 17,

```
libc++abi.dylib: terminating with uncaught exception of type
std::runtime_error: collate_byname::collate_byname failed to construct
for C/en_AU.UTF-8/C/C/C/C
```

use driver 13 or [see here for more detail](https://github.com/fpco/odbc/issues/17).
