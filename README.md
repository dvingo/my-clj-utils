this is a collection of code to help me get things done using clojure and clojurescript.

Might pull these out to separate helper libs later on.

- crux
- tick
- fulcro
- pedestal http server setup with reitit, for use with fulcro
- pathom parser setup

Build a deployable jar of this library:

    $ clojure -A:jar

Install it locally:

    $ clojure -A:install

Deploy it to Clojars -- needs `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment variables:

    $ clojure -A:deploy
