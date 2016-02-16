# resumed

A [Ring](https://github.com/ring-clojure/ring/wiki/Concepts#handlers)
handler to support [tus](http://tus.io/) resumable uploads

## Usage

````clojure
(make-handler opts)
````

### `opts`

* `:save-dir` Path to save location, the path must be writable by the
user running the HTTP service.

## License

Copyright (C) 2016 Akvo Foundation

Distributed under the Mozilla Public License 2.0
