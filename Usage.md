## Start ##

Start server with

> cd oreste
> ./start-dev.sh

## Demo Moodle ##

A module "moodle" is included in the distribution. The following urls can help you to understand better what you can do with oRestE

> User/password: user/pass (see file priv/dispatcher.conf)

```
 http://localhost:8000/moodle/dsn_learninglab/user.xml?username=matteo
 http://localhost:8000/moodle/dsn_learninglab/user.csv?username=matteo
 http://localhost:8000/moodle/dsn_learninglab/user.txt?username=matteo,lengths=30
```

## Add your new module/sql statements ##

Configure your odbc DSN at Operating System Level.

TODO: for windows system, you have to install ODBC drivers and configure them in the "Control panel" ...

For Unix systems (package unixODBC for linux)

File odbcinst.ini

```
[FreeTDS]
Description	= TDS driver (Sybase/MS SQL)
Driver		= /usr/lib/odbc/libtdsodbc.so
Setup		= /usr/lib/odbc/libtdsS.so
CPTimeout	= 
CPReuse		= 

[MySQL]
Description     = Mysql driver
Driver          = /usr/lib/odbc/libmyodbc.so
;Setup           =
CPTimeout       =
CPReuse         =
```

File odbc.ini

```
[LEARNINGLAB]
Driver       = MySQL
Description  = LEARNINGLAB
Server       = localhost
Port         =
User         = moodle
Password     = modole
Database     = moodle
Option       = 3
Socket       =
```

Add your valid ODBC DSN entries in the file priv/dsnpool.conf
For instance

```
   {learninglab, "DSN=LEARNINGLAB", []}.
```


Add the file priv/sqlpool/yourproject.conf with the list of your sql statements you want to expose via http. For instance moodle.conf

```
  {user, 
	"select *
	from
		mdl_user 
	where
		username='{username}'"
  }.
```


Add a line in priv/dispatcher.conf. For instance

```
  {["moodle", dsn, command, '*'], oreste_resource, ["moodle", "user:pass"]}.
```

Http basic Authentication is enabled by default: user:pass are the user/password you need to get the urls

URL

> http://localhost:8000/yourproject/yourdsn/yoursqlcommand.typeofoutput?optionalparam1=value1,..

**type of output** can be:

  * xml
  * csv
  * xls  (semi column separated file)
  * txt: requires a parameter 'lengths' with the list of integers (comma separated) that are the lengths for each field