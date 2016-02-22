;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/

(ns org.akvo.resumed
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.io File FileOutputStream ByteArrayOutputStream]
           java.util.UUID
           javax.xml.bind.DatatypeConverter))

(def tus-headers
  {"Tus-Resumable" "1.0.0"
   "Tus-Version" "1.0.0"
   "Tus-Extension" "creation"
   "Tus-Max-Size" (str (* 1024 1024 50))})

(defonce current-uploads (atom {}))

(defn gen-id []
  (.replaceAll (str (UUID/randomUUID)) "-" ""))

(defn default-headers []
  tus-headers)

(defn options-headers []
  (select-keys tus-headers ["Tus-Version" "Tus-Extension" "Tus-Max-Size"]))

(defn get-header [req header]
  (get-in req [:headers (.toLowerCase ^String header)]))



(defn to-number
  "Returns a numeric representation of a String.
  Returns -1 on unparseable String, blank or nil"
  [s]
  (if (not (str/blank? s))
    (try
      (Long/valueOf ^String s)
      (catch Exception _
        -1))
    -1))

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
                       "Upload-Offset" (str (:offset found))
                       "Upload-Length" (str (:length found))
                       "Upload-Metadata" (:metadata found))}
      {:status 404
       :body "Not Found"})))

(defn patch
  [req opts]
  (let [id (last (str/split (:uri req) #"/"))
        found (@current-uploads id)]
    (if found
      (let [len (-> req (get-header "content-length") to-number)
            off (-> req (get-header "upload-offset") to-number)
            ct (get-header req "content-type")]
        (if (not= "application/offset+octet-stream" ct)
          {:status 400 ;; FIXME: is this the right code?
           :body "Bad request"}
          (if (not= (:offset found) off)
            {:status 409
             :body "Conflict"}
            (with-open [tmp (ByteArrayOutputStream.)
                        fos (FileOutputStream. ^String (:file found) true)]
              (io/copy (:body req) tmp)
              (.write fos (.toByteArray tmp))
              (swap! current-uploads update-in [id :offset] + len)
              {:status 204
               :headers (assoc tus-headers
                               "Upload-Offset" (str (:offset (@current-uploads id))))}))))
      {:status 404
       :body "Not Found"})))

(defmethod handle-request :patch
  [req opts]
  (patch req opts))


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

(defn post
  [req opts]
  (let [len (-> req (get-header "upload-length") to-number)]
    (if (> len (to-number (tus-headers "Tus-Max-Size")))
      {:status 413
       :body "Request Entity Loo Large"}
      (let [id (gen-id)
            um (get-header req "upload-metadata")
            fname (or (get-filename um)  "file")
            fpath (str (:save-path opts) "/" id)
            f (str fpath "/" fname)]
        (swap! current-uploads assoc id {:offset 0 :file f :length len :metadata um})
        (.mkdirs (File. fpath))
        (.createNewFile (File. f))
        {:status 201
         :headers {"Location" (str (get-location req) "/" id)
                   "Upload-Length" (str len) ;FIXME: potentially -1
                   "Upload-Metadata" um      ;FIXME: potentially ""
                   }}))))

(defmethod handle-request :post
  [req opts]
  (let [method-override (get-header req "x-http-method-override")]
    (if (= method-override "PATCH")
      (patch req opts)
      (post req opts))))

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
