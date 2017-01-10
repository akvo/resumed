(defproject org.akvo/resumed "0.2.0"
  :description "A Ring handler to support tus.io protocol"
  :url "http://akvo.org"
  :license {:name "Mozilla Public License 2.0"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :signing {:gpg-key "devops@akvo.org"}
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.cache "0.6.5"]]
  :repositories [["jcenter" {:url "http://jcenter.bintray.com"
                             :snapshots false
                             :checksum :fail
                             :update :always}]]
  :profiles {:dev {:resource-paths ["test/resources"]
                   :dependencies [[ring/ring-jetty-adapter "1.4.0"]
                                  [ring/ring-mock "0.3.0"]
                                  [io.tus.java.client/tus-java-client "0.3.1"]]}})
