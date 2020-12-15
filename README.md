# works-sprig

`client/src/Config.elm`

```elm
module Config exposing (endpoint)
endpoint = "http://localhost:3000/tasks"
```

`server/pgconf.yaml`

```yaml
user    : "_env:PG_USER:{}"
password: "_env:PG_PASS:{}"
host    : "_env:PG_HOST:{}"
port    : "_env:PG_PORT:{}"
database: "_env:PG_DATABASE:{}"
poolsize: "_env:PG_POOLSIZE:{}"
```
