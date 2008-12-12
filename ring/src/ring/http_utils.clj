(ns ring.http-utils
  (:use clojure.contrib.def
        clojure.contrib.str-utils
        clojure.contrib.except))

(defn url-escape
  "Returns a url-escaped representation of the given String."
  [unescaped]
  (java.net.URLEncoder/encode unescaped "UTF-8"))

(defn url-unescape
  "Returns a url-unescaped representation of the given String."
  [escaped]
  (java.net.URLDecoder/decode escaped "UTF-8"))

(defn- assoc-last
  [coll val]
  (if (empty? coll)
    [val]
    (assoc coll (dec (count coll)) val)))

(defvar- after-initial-key-pat #"^([^\[]*)(\[.*)$"
  "Matches keys of the form key-head([..key-trail..)")

(defvar- hash-key-pat          #"^\[([^\]]+)\](.*)$"
  "Matches keys of the form [key-head](..key-tail..)")

(defvar- nested-hash-key-pat   #"^\[\]\[([^\]]+)\](.*)$"
  "Matches keys of the form [][key-head](..key-tail..)")

; cases - "", "[]", "[foo]...", "[][foo]..."
(defn- pairs-parse-nested
  [params key value]
  (if (= key "")
    value
    (if (= key "[]")
      (conj (or params []) value)
      (if-let [found (re-find hash-key-pat key)]
        (let [key-head (keyword (nth found 1))
              key-rest (nth found 2)]
          (assoc params key-head
            (pairs-parse-nested (get params key-head) key-rest value)))
        (if-let [found (re-find nested-hash-key-pat key)]
          (let [key-head   (keyword (nth found 1))
                key-rest   (nth found 2)
                last-inner (last params)]
            (cond
              (not last-inner)
                [{key-head (pairs-parse-nested nil key-rest value)}]
              (contains? last-inner key-head)
                (conj params
                  {key-head (pairs-parse-nested nil key-rest value)})
              :else
                (assoc-last params
                  (assoc last-inner key-head
                    (pairs-parse-nested (get last-inner key-head)
                      key-rest value)))))
          (throwf "Unrecognized key: %s" key))))))

(defn pairs-parse
  "Returns a potentially-nested data structure corresponding to the given
  name, value pairs."
  [pairs]
  (let [non-empty (remove #(nil? (second %)) pairs)]
    (reduce
      (fn [params [key value]]
        (if-let [found (re-find after-initial-key-pat key)]
          (let [key-head (keyword (nth found 1))
                key-rest (nth found 2)]
            (assoc params key-head
              (pairs-parse-nested
                (get params key-head) key-rest value)))
          (assoc params (keyword key) value)))
      {}
      non-empty)))

(defn- querylike-parse
  [separator string]
  (if string
    (let [segments  (re-split separator string)
          unescaped (map url-unescape segments)
          pairs     (map #(re-split #"=" % 2) unescaped)]
      (pairs-parse pairs))))

(defn query-parse
  "Returns a potentially-nested data structure corresponding to the given
  query string."
  [query-string]
  (querylike-parse #"&\s*" query-string))

(defn cookie-parse
  "Returns a non-nested map of cookie values corresponding to the given Cookie
  header string, or nil if the given value is nil."
  [cookie-string]
  (querylike-parse #";\s*" cookie-string))