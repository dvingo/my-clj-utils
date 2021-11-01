
[![Clojars Project](https://img.shields.io/clojars/v/dv/clj-utils.svg)](https://clojars.org/dv/clj-utils)

this is a collection of code to help me get things done using clojure and clojurescript.

Contains some code related to:

- CRUX (and the equivalent XTDB helpers)
- fulcro3
  - devcards support
- pedestal http server setup with reitit, for use with fulcro
- pathom parser setup

Build a deployable jar of this library:

  
```bash
make jar
```

Install it locally:

```bash
clojure -A:install
```

Deploy it to Clojars -- needs `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment variables:

```bash
clojure -A:deploy
```

# Deploy notes for clojars

1. Update the version of the maven package in pom.xml - and git commit it.
2. Build the jar via `make`
3. CLOJARS_USERNAME='' CLOJARS_PASSWORD='deploy_token' clojure -A:deploy 
