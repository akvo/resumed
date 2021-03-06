(ns org.akvo.resumed-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [org.akvo.resumed :refer :all]
            [ring.adapter.jetty :as jetty]
            [ring.mock.request :as m])
  (:import [io.tus.java.client TusClient TusUpload
            TusUploader TusURLMemoryStore]
           java.io.File
           java.net.URL))

(defn res-to-byte-array
  "Reads a resource file to a byte array
  Attribution: http://stackoverflow.com/a/26791567"
  ([path off len]
   (let [f (io/file (io/resource path))
         ba (byte-array len)
         is (io/input-stream f)]
     (.read is ba off len)
     (.close is)
     ba))
  ([path]
   (let [f (io/file (io/resource path))
         ba (byte-array (.length f))
         is (io/input-stream f)]
     (.read is ba)
     (.close is)
     ba)))

(deftest test-utilities
  (testing "Testing utility functions"
    (is (= "one.pdf" (get-filename "filename b25lLnBkZg==,meta2 ZHVtbXk=")))
    (is (= true (nil? (get-filename ""))))
    (is (= true (nil? (get-filename nil))))
    (is (= "world_domination_plan.pdf" (get-filename "filename d29ybGRfZG9taW5hdGlvbl9wbGFuLnBkZg==")))
    (is (= "max-age=0" (get-header {:params {} :headers {"tus-resumable" "1.0.0"
                                                         "connection"    "keep-alive"
                                                         "cache-control" "max-age=0"}} "Cache-Control")))
    (is (= "https://mysecure-host.org/files" (location (m/request :get "https://mysecure-host.org/files"))))
    (is (= "http://localhost:3000/files" (location (m/request :get "http://localhost:3000/files"))))
    (is (= "https://some-secure-server/files" (location (m/request :get "https://some-secure-server:443/files"))))
    (is (= "http://localhost/files" (location (m/request :get "http://localhost:80/files"))))))

(deftest test-options
  (let [handler (make-handler)
        req (m/request :options "http://localhost:3000/files")
        resp (handler req)]
    (testing "OPTIONS"
      (is (= 204 (:status resp)))
      (is (= 3 (count (keys (:headers resp)))))
      (is (= true (nil? (get-in resp [:headers "Tus-Resumable"])))))))

(deftest good-post
  (let [handler (make-handler)
        um "filename d29ybGRfZG9taW5hdGlvbl9wbGFuLnBkZg==,meta2 ZHVtbXk="
        req (-> (m/request :post "http://localhost:3000/files")
                (m/header "Upload-Metadata" um)
                (m/header "Upload-Length" 10))
        resp (handler req)
        location (get-in resp [:headers "Location"])
        metadata (get-in resp [:headers "Upload-Metadata"])]
    (testing "POST"
      (is (= 201 (:status resp)))
      (is (= true (.startsWith location "http://localhost:3000/files/")))
      (is (= true (.exists (file-for-upload nil location))))
      (is (= 0 (.length (file-for-upload nil location))))
      (is (= um metadata)))))

(deftest test-good-head
  (let [handler (make-handler)
        um "filename cGcxMS50eHQ="
        len 167518
        post (-> (m/request :post "http://localhost:3000/files")
                 (m/header "Upload-Metadata" um)
                 (m/header "Upload-Length" len))
        resp (handler post)
        head (-> (m/request :head (get-in resp [:headers "Location"]))
                 (m/header "Tus-Version" "1.0.0"))
        resp (handler head)]
    (testing "HEAD"
      (is (= 200 (:status resp)))
      (is (= (str len) (get-in resp [:headers "Upload-Length"])))
      (is (= um (get-in resp [:headers "Upload-Metadata"])))
      (is (= "0" (get-in resp [:headers "Upload-Offset"])))
      (is (= "no-cache" (get-in resp [:headers "Cache-Control"]))))))

(deftest test-head-not-found
  (let [handler (make-handler)
        head (m/request :head "http://localhost:3000/file/some-file")
        resp (handler head)]
    (testing "HEAD not found"
      (is (= 404 (:status resp)))
      (is (= "no-cache" (get-in resp [:headers "Cache-Control"]))))))

(deftest patch-single-request
  (let [handler (make-handler)
        um "filename cGcxMS50eHQ="
        ba (res-to-byte-array "resources/pg11.txt")
        len (count ba)
        post (-> (m/request :post "http://localhost:3000/files")
                 (m/header "Upload-Metadata" um)
                 (m/header "Upload-Length" len))
        resp (handler post)
        location (get-in resp [:headers "Location"])
        patch (-> (m/request :patch location)
                  (m/header "Content-Type" "application/offset+octet-stream")
                  (m/header "Content-Length" len)
                  (m/header "Upload-Offset" 0)
                  (m/body ba))
        resp (handler patch)]
    (testing "PATCH in single request"
      (is (= 204 (:status resp)))
      (is (= (str len) (get-in resp [:headers "Upload-Offset"]))))))

(deftest patch-multiple-requests
  (let [handler (make-handler)
        um "filename cGcxMS50eHQ="
        ba (res-to-byte-array "resources/pg11.txt")
        len (count ba)
        size 1024
        post (-> (m/request :post "http://localhost:3000/files")
                 (m/header "Upload-Metadata" um)
                 (m/header "Upload-Length" len))
        resp (handler post)
        location (get-in resp [:headers "Location"])
        parts (partition-all size ba)]
    (testing "PATCH multiple requests"
      (loop [offset 0
             data parts]
        (when (seq data)
          (let [body (first data)
                rlen (count body)
                patch (-> (m/request :patch location)
                          (m/header "Content-Type" "application/offset+octet-stream")
                          (m/header "Content-Length" rlen)
                          (m/header "Upload-Offset" offset)
                          (m/body (byte-array body)))
                resp (handler patch)]
            (is (= 204 (:status resp)))
            (is (= (str (+ offset rlen)) (get-in resp [:headers "Upload-Offset"]))))
          (recur (+ offset (count (first data))) (rest data)))))))

(deftest file-too-big
  (let [handler (make-handler {:max-upload-size 0.1})
        req (-> (m/request :post "http://localhost:3000/files")
                (m/header "Upload-Length" 1000000))
        resp (handler req)
        location (get-in resp [:headers "Location"])]
    (testing "POST"
      (is (= 413 (:status resp)))
      (is (nil? location)))))

(defn jetty-port [jetty]
  (-> jetty
      .getConnectors
      first
      .getLocalPort))

(deftest java-client
  (testing "Java client"
    (let [handler (make-handler)
          srv (jetty/run-jetty handler {:port 0 :join? false})]
      (try
        (let [port (jetty-port srv)
              client (TusClient.)
              _ (.setUploadCreationURL client (URL. (format "http://localhost:%s/" port)))
              _ (.enableResuming client (TusURLMemoryStore.))
              f (io/file (io/resource "resources/pg11.txt"))
              upload (TusUpload. f)
              uploader (.resumeOrCreateUpload client upload)
              chunk-size 1024
              _ (.setChunkSize uploader chunk-size)]
          (loop [up (.uploadChunk uploader)]
            (when (> up -1)
              (is (> (.getOffset uploader) 0))
              (recur (.uploadChunk uploader))))
          (.finish uploader))
        (finally
          (.stop srv))))))

(deftest java-client-resume
  (testing "Java client - resuming"
    (let [handler (make-handler)
          srv (jetty/run-jetty handler {:port 0 :join? false})]
      (try
        (let [port (jetty-port srv)
              client (TusClient.)
              _ (.setUploadCreationURL client (URL. (format "http://localhost:%s/" port)))
              _ (.enableResuming client (TusURLMemoryStore.))
              f (io/file (io/resource "resources/pg11.txt"))
              limit (/ (.length f) 2)
              upload1 (TusUpload. f)
              upldr (.resumeOrCreateUpload client upload1)
              chunk-size 1024
              _ (.setChunkSize upldr chunk-size)
              _ (loop [up (.uploadChunk upldr)]             ;; upload first half
                  (when (and (> up -1) (< (.getOffset upldr) limit))
                    (recur (.uploadChunk upldr))))
              _ (.finish upldr)
              upload2 (TusUpload. f)                        ;; resume
              upldr2 (.resumeOrCreateUpload client upload2)
              _ (.setChunkSize upldr2 chunk-size)]
          (loop [up (.uploadChunk upldr2)]
            (when (> up -1)
              (is (> (.getOffset upldr2) 0))
              (recur (.uploadChunk upldr2))))
          (.finish upldr2)
          (.stop srv))
        (finally
          (.stop srv))))))

(deftest google-cloud-load-balancer-complience
  (testing "Lack of x-forwarded-host should fallback on host header"
    (are [x y] (= x (location y))
      "https://www.akvo.org/path" {:headers {"host" "www.akvo.org"
                                             "x-forwarded-proto" "https"}
                                   :uri "/path"
                                   :server-port 3000
                                   :scheme :http}
      "http://www.akvo.org:3000/path" {:headers {"host" "www.akvo.org"}
                                       :uri "/path"
                                       :server-port 3000
                                       :scheme :http}
      "http://localhost:3030/path" {:headers {"host" "localhost:3030"}
                                    :uri "/path"
                                    :server-port 4000
                                    :scheme :http}
      "http://some.tld:4000/path" {:headers {"host" "some.tld"}
                                   :uri "/path"
                                   :server-port 4000
                                   :scheme :http}
      "https://some.tld:8443/path" {:headers {"host" "some.tld"}
                                    :uri "/path"
                                    :server-port 8443
                                    :scheme :https}
      "http://t1.lumen.local:3030/api/files" {:headers {"host" "t1.lumen.local:3030"
                                                        "origin" "http://t1.lumen.local:3030"}
                                              :uri "/api/files"
                                              :server-port 3000
                                              :scheme :http})))


(deftest check-upload-length
  (testing "Restrict the number of PATCH requests to the Upload-Length"
    (let [ba (res-to-byte-array "resources/pg11.txt")
          len (count ba)
          handler (make-handler {:max-upload-size 0.5})
          um "filename cGcxMS50eHQ="
          post-req (-> (m/request :post "http://localhost:3000/files")
                       (m/header "Upload-Metadata" um)
                       (m/header "Upload-Length" len))
          post-res (handler post-req)
          location (get-in post-res [:headers "Location"])]
      (dotimes [n 10]
        (let [patch-req (-> (m/request :patch location)
                            (m/header "Content-Type" "application/offset+octet-stream")
                            (m/header "Content-Length" len)
                            (m/header "Upload-Offset" (* n len))
                            (m/body ba))
              patch-res (handler patch-req)]
          (when (zero? n)
            (is (= 204 (:status patch-res)))
            (is (= (str (* (inc n) len)) (get-in patch-res [:headers "Upload-Offset"]))))
          (when-not (zero? n)
            (is (not= 204 (:status patch-res)))))))))
