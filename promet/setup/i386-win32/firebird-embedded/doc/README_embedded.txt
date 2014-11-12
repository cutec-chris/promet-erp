-----------------------------------------------------------
Firebird 2.1 Embedded Server notes
-----------------------------------------------------------

1. GENERIC INFORMATION

  The embedded server is a fully functional server linked
  as a dynamic library (fbembed.dll). It has exactly the
  same features as the usual server and exports the
  standard Firebird API entrypoints.

2. ISSUES AND LIMITATIONS

  2.1. Registry

    The Firebird registry entries are ignored. The root
    directory of the embedded server is the directory of
    its binary file (library).

  2.2. Database access

    Client access can be only via the local protocol, 
    i.e. NOT a TCP/IP connection string that includes 
    the server name "localhost" or IP address 127.0.0.1. 

    The embedded server supports only the local connect
    to a database file path without a server name. The
    client program gets an exclusive access to the
    database file after successful connect.

    The embedded server acts as a true local server for a single
    client accessing databases on a local machine.  It can also
    act as a remote gateway that redirects all network calls to
    other hosts, just as the regular client library
    does.

  2.3. Authentication and security

    The security database (namely security2.fdb) is not used
    in the embedded server and hence is not required. Any
    user is able to attach to any database. Since both
    the server and the client run in the same address space,
    the security becomes just an agreement between both
    sides which can be easily compromised.

    But note that SQL privileges are still checked.

  2.4. Compatibility

    You may run any number of applications with the embedded
    server without any conflicts. Having IB/FB server running
    is not a problem either.

    But you should be aware that you cannot access single
    database from a number of the embedded servers
    simultaneously, because they have SuperServer architecture
    and hence exclusively lock attached databases.

3. USAGE

  Just copy fbembed.dll, icudt30.dll, icuin30.dll and
  icuuc30.dll into the directory with your application.
  Then rename fbembed.dll to either fbclient.dll or
  gds32.dll depending on your database connectivity software.
  Then start your application and it will use the embedded
  server as a client library and will be able to access
  local datasases. You should also copy firebird.msg and
  firebird.conf (if necessary) to the same directory.

  If external libraries are required for your application,
  then you should have them separately. Most probably, it
  will be INTL support (fbintl.dll and fbintl.conf) or UDF
  libraries. To be able to use them, you should place them
  into the directory tree which emulates the Firebird server
  one, i.e. has subdirectories like /intl or /udf:

  c:\my_app\app.exe
  c:\my_app\gds32.dll
  c:\my_app\ib_util.dll
  c:\my_app\icudt30.dll
  c:\my_app\icuin30.dll
  c:\my_app\icuuc30.dll
  c:\my_app\firebird.conf
  c:\my_app\firebird.msg
  c:\my_app\intl\fbintl.dll
  c:\my_app\intl\fbintl.conf
  c:\my_app\udf\fbudf.dll

  If you want to place the Firebird files (excluding the
  renamed fbembed.dll) in another directory, you need to
  modify your firebird.conf and set RootDirectory to the
  Firebird directory tree. Example:

  c:\my_app\app.exe
  c:\my_app\gds32.dll
  c:\my_app\ib_util.dll
  c:\my_app\icudt30.dll
  c:\my_app\icuin30.dll
  c:\my_app\icuuc30.dll
  c:\my_app\firebird.conf
  d:\fb\firebird.msg
  d:\fb\intl\fbintl.dll
  c:\fb\intl\fbintl.conf
  d:\fb\udf\fbudf.dll

  firebird.conf:
  RootDirectory = d:\fb
