| ![http://www.erlang.org/images/erlang.gif](http://www.erlang.org/images/erlang.gif) | ![http://www.redaelli.org/matteo/blog/wp-content/uploads/2009/10/oreste1.png](http://www.redaelli.org/matteo/blog/wp-content/uploads/2009/10/oreste1.png) |
|:------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------|

**oRestE** gives you a fast way to expose your **databases** with a **REST interface**.

The output can be in **XML**, **CSV**, **XLS** (csv with ; as separator) or **fixed size**

**oRestE** is built on top of [ERLANG](http://www.erlang.org/) ([Introduction](http://www.slideshare.net/kenpratt/intro-to-erlang)), [MOCHIWEB](http://code.google.com/p/mochiweb/) and [WEBMACHINE](http://webmachine.basho.com/) ([webmachine.pdf](http://www.erlang-factory.com/upload/presentations/60/sheehy_factory-webmachine.pdf))

**oRestE** exposes sql queries using HTTP GET requests like

  * http://localhost:8000/moodle/learninglab/user.xml?username=matteo
  * http://localhost:8000/moodle/learninglab/user.csv?username=matteo
  * http://localhost:8000/moodle/learninglab/user.xls?username=matteo
  * http://localhost:8000/moodle/learninglab/user.txt?username=matteo&lengths=30


You simply need to add in a configuration file your sql queries like

```
  {user, 
	"select *
	from
		mdl_user 
	where
		username ='{username}'"
  }.
```

### Admin commands ###

  * http://localhost:8000/admin/status : show active DSN and SQL processes and statistics about requests
  * http://localhost:8000/admin/db_reconnect?dsn=learninglab : disconnect and connect to the DSN learninglab
  * http://localhost:8000/admin/sql_reload?sql=moodle : reload the sql statements from file moodle.conf

> User/password: user/pass (see file priv/dispatcher.conf)

### WIKI ###

  * [Installation](Installation.md)
  * [Usage](Usage.md)
  * [Users](Users.md)