# pgbadger

### Using rebar to create OTP application

```bash
rebar create-app appid=myapp
rebar create template=simplesrv srvid=myapp_srv
rebar compile
```

### Start an application from command line
```bash
erl -pa ebin -pz deps/*/ebin
```

```erlang
application:start(myapp)
```

### Add dependencies

Create rebar.config file:

```erlang
{deps, [
	{pgsql, ".*", {git, "https://github.com/semiocast/pgsql.git"}}
]}.
```

```bash
rebar get-deps
rebar compile
```
