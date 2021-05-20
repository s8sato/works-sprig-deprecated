# works-sprig-deprecated

[new]: https://github.com/satu-n/works-sprig
[servant]: https://www.servant.dev/

## What's this

* A repository just to show my experience with Haskell/[Servant][servant]
* This works, but [here][new]'s a rework

## How to run

`client/src/Config.elm`

```elm
module Config exposing (endpoint)
endpoint = "http://localhost:3333/tasks"
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
