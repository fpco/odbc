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

You can use the [SQL Server docker image](https://hub.docker.com/_/microsoft-mssql-server) to easily run SQL Server anywhere in a few seconds.

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

## Check your package is working

You can spin up a SQL Server in docker and connect to it with the
trivial binary `odbc` that comes with this package:

```
$ docker run --net=host -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=Passw0rd' -d mcr.microsoft.com/mssql/server:2017-CU8-ubuntu
Unable to find image 'mcr.microsoft.com/mssql/server:2017-CU8-ubuntu' locally
2017-CU8-ubuntu: Pulling from mssql/server
4fa80d7b805d: Pull complete
484dd0f2fbdc: Pull complete
47004b22ec62: Pull complete
b70745c852a2: Pull complete
718060832ef2: Pull complete
5594e4e5950b: Pull complete
5b67719e2956: Pull complete
7d648891de3f: Pull complete
e0d1b3db20c8: Pull complete
ded313a21911: Pull complete
Digest: sha256:e1708b7d3aaf4a693ef8785f15a8b4d082939681e373c4090fd0b294d1501e57
Status: Downloaded newer image for mcr.microsoft.com/mssql/server:2017-CU8-ubuntu
ba1ad8b726c7e958bad6d2f7b051514f218c3024984f388adab2d6bb7751ea90

$ stack exec odbc 'DRIVER={ODBC Driver 17 for SQL Server};SERVER=127.0.0.1;Uid=SA;Pwd=Passw0rd;Encrypt=no'
> select 2 * 3;
6
Rows: 1
>
$
```

## Common issues


### Can't open lib 'ODBC Driver 13 for SQL Server'

If you see an error like this:

    [unixODBC][Driver Manager]Can't open lib 'ODBC Driver 13 for SQL Server' : file not found

Then you might be trying to use the wrong driver. You might have
installed version `17`, so change the string to `ODBC Driver 17 for
SQL Server`.

If it still says this, you might have to configure an odbcinst.ini
file:

``` yaml
[ODBC Driver 17 for SQL Server]
Driver = <driver_path>
```

In Nix, this might be where <driver_path> is the result of evaluating
`${nixpkgs.unixODBCDrivers.msodbcsql17}/lib/libmsodbcsql-17.7.so.1.1"`.

Which would need the following packages available:

* nixpkgs.freetds
* nixpklgs.unixODBC
* nixpkgs.unixODBCDrivers.msodbcsql17

### Data source name not found and no default driver specified

If you see an error like this:

    [unixODBC][Driver Manager]Data source name not found and no default driver specified

This is a terrible error message. If passing your DSN via a shell
environment variable or argument, check that your input string isn't
quoted e.g. `"Driver=.."` instead of `Driver=..` due to silly shell
scripting quoting issues.

## Terminating with uncaught exception of type

If you see an error like this on OS X with driver version 17,

```
libc++abi.dylib: terminating with uncaught exception of type
std::runtime_error: collate_byname::collate_byname failed to construct
for C/en_AU.UTF-8/C/C/C/C
```

use driver 13 or [see here for more detail](https://github.com/fpco/odbc/issues/17).

## Contributors

* Spencer Janssen
* Yo Eight
* Marco Z
