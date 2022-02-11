webxec: execute a known command for every HTTP request received (a.k.a, cheaplambda)

:warning: This is a developement support tool, and at best a power-user
debugging or desktop tool. I would summarize webxec as a _modest remote code
execution system_.
You, may not want take a close look at security risks (RCE and data leaks
risks) before running this in production.

## why?

Because a number of reasons:
- cloud-functions are trendy
- CGI-bins are vintage

More pragmatically, a recent example where it was useful: I use this to get
some desktop notifications while developing web services.  For instance, I add
a trigger on a locally-running [Hasura](https://hasura.io/) instance, I define
a notification trigger to target my locally-running `webxec`, which forwards to
`notify-send`.

## build

```
cabal build
```

## behavior

- Returns a 200 if the command execution returns anything.
- Hangs forever if the command execution never ends.
- Concatenates the stdout and stderr of the command in the HTTP response body.
- Returns HTTP headers as follows:
  - `X-webxec-code` : return-code as a shown Haskell (`ExitSuccess` for the happy case)
  - `X-webxec-out-length` : content-length of the stdout data
  - `X-webxec-err-length` : content-length of the stderr data


## example usage

```
webxec --portNum 9329 \
  --command notify-send \
  --prefixArg=-i \
  --prefixArg=network-receive \
  --argStyle=CONCAT_ARGS \
  --payloadStyle=NO_PAYLOAD
``

### explanation of args

The `--argStyle` provides a mapping from the HTTP query data to CLI arguments.
The `--payloadStyle` provides a mapping from the HTTP query data to CLI standard input as a JSON object with the following keys: `method`, `path`, `query`, `headers`, `body`.

Possible `--argStyle`
- `NO_ARGS` : 0 arguments
- `BASIC_ARGS` : 3 arguments in this order "${http_method}" "${http_path}" "${http_queryparams}"
- `CONCAT_ARGS` : 1 argument concatenating the three above "${http_method} ${http_path} ${http_queryparams}"

Possible `--payloadStyle`
- `NO_PAYLOAD` : empty string
- `ALL_PAYLOAD` : a json object with all keys filled-in
- `BASIC_PAYLOAD` : a json object with all keys filled-in except the HTTP body (which is not read from the HTTP connection)
