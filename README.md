
[![Clojars Project](https://img.shields.io/clojars/v/dv/clj-utils.svg)](https://clojars.org/dv/clj-utils)

this is a collection of code to help me get things done using clojure and clojurescript.

Contains some code related to:

- crux
- fulcro
  - devcards support
- pedestal http server setup with reitit, for use with fulcro
- pathom parser setup

Build a deployable jar of this library:

    $ clojure -A:jar

Install it locally:

    $ clojure -A:install

Deploy it to Clojars -- needs `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment variables:

    $ clojure -A:deploy
