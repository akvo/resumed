(defproject org.akvo.resumed "0.1.0-SNAPSHOT"
  :description "A Ring handler to support tus.io protocol"
  :url "http://akvo.org"
  :license {:name "Mozilla Public License 2.0"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :profiles {:dev {:dependencies [[ring/ring-mock "0.3.0"]]}})
