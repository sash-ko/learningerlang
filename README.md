# learningerlang

### Using rebar to create OTP application

```bash
rebar create-app appid=myapp
rebar create template=simplesrv srvid=myapp_srv
rebar compile
```

### Start an application from command line
```bash
erl -pa ebin
```

```erlang
application:start(myapp)
```
