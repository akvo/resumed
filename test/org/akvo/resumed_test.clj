(ns org.akvo.resumed-test
  (:require [clojure.test :refer :all]
            [org.akvo.resumed :refer :all]
            [ring.mock.request :as m]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty])
  (:import java.io.File
           java.net.URL
           [io.tus.java.client TusClient TusUpload
                               TusUploader TusURLMemoryStore]))

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
    (is (= "https://mysecure-host.org/files" (get-location (m/request :get "https://mysecure-host.org/files"))))
    (is (= "http://localhost:3000/files" (get-location (m/request :get "http://localhost:3000/files"))))
    (is (= "https://some-secure-server/files" (get-location (m/request :get "https://some-secure-server:443/files"))))
    (is (= "http://localhost/files" (get-location (m/request :get "http://localhost:80/files"))))))

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
        head (-> (m/request :head "http://localhost:3000/file/some-file"))
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

(defn port [jetty]
  (-> jetty
      .getConnectors
      first
      .getLocalPort))

(deftest java-client
  (testing "Java client"
    (let [handler (make-handler)
          srv (jetty/run-jetty handler {:port 0 :join? false})]
      (try
        (let [port (port srv)
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
        (let [port (port srv)
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
    (let [req {:headers {"accept"                "*/*"
                         "accept-encoding"       "gzip, deflate"
                         "connection"            "Keep-Alive"
                         "host"                  "www.akvo.org"
                         "user-agent"            "."
                         "via"                   "1.1 google"
                         "x-cloud-trace-context" ""
                         "x-forwarded-for"       "0.0.0.0, 127.0.0.1"
                         "x-forwarded-proto"     "https"}
               :uri     "/path"}]
      (is (= "https://www.akvo.org/path"
             (get-location req))))))
