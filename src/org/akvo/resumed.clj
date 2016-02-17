;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/

(ns org.akvo.resumed
  (:require [clojure.string :as str])
  (:import java.io.File
           java.util.UUID
           javax.xml.bind.DatatypeConverter))

(def tus-headers
  {"Tus-Resumable" "1.0.0"
   "Tus-Version" "1.0.0"
   "Tus-Extension" "creation"
   "Tus-Max-Size" (* 1024 1024 50)})

(defonce current-uploads (atom {}))

(defn gen-id []
  (.replaceAll (str (UUID/randomUUID)) "-" ""))

(defn default-headers []
  tus-headers)

(defn options-headers []
  (select-keys tus-headers ["Tus-Version" "Tus-Extension" "Tus-Max-Size"]))

(defn get-header [req header]
  (get-in req [:headers (.toLowerCase ^String header)]))

(defmulti handle-request
  (fn [req opts]
    (:request-method req)))

(defmethod handle-request :default
  [req opts]
  {:status 400})

(defmethod handle-request :options
  [req opts]
  {:status 204
   :headers (options-headers)})

(defmethod handle-request :head
  [req opts]
  (let [upload-id (last (str/split (:uri req) #"/"))]
    (if-let [found (@current-uploads upload-id)]
      {:status 200
       :headers (assoc tus-headers
                       "Upload-Offset" (:offset found)
                       "Upload-Length" (:length found)
                       "Upload-Metadata" (:metadata found))}
      {:status 404
       :body "Not Found"})))

(defmethod handle-request :patch
  [req opts]
  {:status 204})

(defn get-filename
  "Returns a file name decoding a base64 string of
  Upload-Metadata header.
  Attribution: http://stackoverflow.com/a/2054226"
  [s]
  (when-not (str/blank? s)
    (let [m (->> (str/split s #",")
                 (map #(str/split % #" "))
                 (into {}))]
      (when (contains? m "filename")
        (-> (m "filename")
            (DatatypeConverter/parseBase64Binary)
            (String.))))))

(defn get-location
  "Get Location string from request"
  [req]
  (format "%s://%s%s%s"
          (name (:scheme req))
          (:server-name req)
          (if (and (not= (:server-port req) 80)
                   (not= (:server-port req) 443))
            (str ":" (:server-port req))
            "")
          (:uri req)))

(defn get-upload-length
  "Returns a number based on the Upload-Length
  request header"
  [req]
  (let [len (get-header req "upload-length")]
    (if len
      (try
        (Long/valueOf ^String len)
        (catch Exception _
          -1))
      -1)))

(defmethod handle-request :post
  [req opts]
  (let [len (get-upload-length req)]
    (if (> len (tus-headers "Tus-Max-Size"))
      {:status 413
       :body "Request Entity Loo Large"}
      (let [id (gen-id)
            um (get-header req "upload-metadata")
            fname (or (get-filename um)  id)
            fpath (str (:save-path opts) "/" id)]
        (swap! current-uploads assoc id {:offset 0 :filename fname :length len :metadata um})
        (.mkdirs (File. fpath))
        (.createNewFile (File. (str fpath "/" fname)))
        {:status 201
         :headers {"Location" (str (get-location req) "/" id)
                   "Upload-Length" len  ;FIXME: potentially -1
                   "Upload-Metadata" um ;FIXME: potentially ""
                   }}))))

(defn make-handler
  "Returns a ring handler capable of responding to client requests from
  a `tus` client. An optional map with configuration can be used
  {:save-dir \"/path/to/save/dir\"} defaults to `java.io.tmpdir`"
  [& [opts]]
  (let [save-dir (or (:save-dir opts)
                     (System/getProperty "java.io.tmpdir"))
        save-path (File. (str save-dir "/resumed"))]
    (.mkdirs save-path)
    (fn [req]
      (handle-request req (assoc opts :save-path (str save-path))))))
