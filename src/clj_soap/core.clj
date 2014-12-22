(ns clj-soap.core
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.core.incubator :refer [-?>]])
  (:import [org.apache.axis2.transport.http HTTPConstants]))

;;; Defining SOAP Server

(defn flatten1 [coll] (mapcat identity coll))

(defn gen-class-method-decls [method-defs]
  (flatten1
    (letfn [(gen-decl [method-name arglist body]
              [method-name
               (vec (for [arg arglist] (or (:tag (meta arg)) String)))
               (or (:tag (meta arglist)) 'void)])]
      (for [method-def method-defs]
        (cond
          (vector? (second method-def))
          (list (let [[method-name arglist & body] method-def]
                  (gen-decl method-name arglist body)))
          (seq? (second method-def))
          (let [[method-name & deflist] method-def]
            (for [[arglist & body] deflist]
              (gen-decl method-name arglist body))))))))

(defn gen-method-defs [prefix method-defs]
  (flatten1
    (for [method-def method-defs]
      (cond
        (vector? (second method-def))
        (list (let [[method-name arglist & body] method-def]
                `(defn ~(symbol (str prefix method-name))
                   ~(vec (cons 'this arglist)) ~@body)))
        (seq? (second method-def))
        (let [[method-name & deflist] method-def]
          (cons
            `(defmulti ~(symbol (str prefix method-name))
               (fn [~'this & args#] (vec (map class args#))))
            (for [[arglist & body] deflist]
              `(defmethod ~(symbol (str prefix method-name))
                 ~(vec (map #(:tag (meta %)) arglist))
                 ~(vec (cons 'this arglist)) ~@body))))))))


(defmacro defservice
  "Define SOAP class.
  i.e. (defsoap some.package.KlassName (myfunc [String a int b] String (str a (* b b))))"
  [class-name & method-defs]
  (let [prefix (str (gensym "prefix"))]
    `(do
       (gen-class
         :name ~class-name
         :prefix ~prefix
         :methods ~(vec (gen-class-method-decls method-defs)))
       ~@(gen-method-defs prefix method-defs))))

(defn serve
  "Start SOAP server.
  argument classes is list of strings of classnames."
  [& classes]
  (let [server (org.apache.axis2.engine.AxisServer.)]
    (doseq [c classes]
      (.deployService server (str c)))))

;; Client call

(defn axis-service-namespace [axis-service]
  (.get (.getNamespaceMap axis-service) "ns"))

(defn axis-service-operations [axis-service]
  (iterator-seq (.getOperations axis-service)))

(defn axis-op-name [axis-op]
  (.getLocalPart (.getName axis-op)))

(defn axis-op-namespace [axis-op]
  (.getNamespaceURI (.getName axis-op)))

(defn axis-op-args [axis-op]
  (for [elem (-?> (first (filter #(= "out" (.getDirection %))
                                 (iterator-seq (.getMessages axis-op))))
                  .getSchemaElement .getSchemaType
                  .getParticle .getItems .getIterator iterator-seq)]
    {:name (.getName elem) :type (-?> elem .getSchemaType .getName keyword)
     :elem elem}))

(defn axis-op-rettype [axis-op]
  (-?> (first (filter #(= "in" (.getDirection %))
                      (iterator-seq (.getMessages axis-op))))
       .getSchemaElement .getSchemaType .getParticle .getItems .getIterator
       iterator-seq first
       .getSchemaType .getName
       keyword))

(defmulti obj->soap-str (fn [obj argtype] argtype))

(defmethod obj->soap-str :integer [obj argtype] (str obj))
(defmethod obj->soap-str :double [obj argtype] (str obj))
(defmethod obj->soap-str :long [obj argtype] (str obj))
(defmethod obj->soap-str :string [obj argtype] (str obj))
(defmethod obj->soap-str :boolean [obj argtype] (str obj))
(defmethod obj->soap-str :default [obj argtype] (str obj))

(defmulti soap-str->obj (fn [obj argtype] argtype))

(def multi-parser (f/formatter (t/default-time-zone) "dd/MM/YYYY HH:mm:ss a" "YYYY-MM-dd" "YYYY/MM/dd"))

(defmethod soap-str->obj :integer [soap-str argtype] (Integer/parseInt soap-str))
(defmethod soap-str->obj :double [soap-str argtype] (Double/parseDouble soap-str))
(defmethod soap-str->obj :long [soap-str argtype] (Long/parseLong soap-str))
(defmethod soap-str->obj :string [soap-str argtype] soap-str)
(defmethod soap-str->obj :boolean [soap-str argtype] (Boolean/parseBoolean soap-str))
(defmethod soap-str->obj :date [soap-str argtype] (f/parse multi-parser soap-str))
(defmethod soap-str->obj :default [soap-str argtype] soap-str)

(defn make-om-elem
  ([factory xml-namespace tag-name]
   (.createOMElement
           factory (javax.xml.namespace.QName. xml-namespace tag-name)))
  ([factory xml-namespace tag-name value]
   (doto (.createOMElement
           factory (javax.xml.namespace.QName. xml-namespace tag-name))
     (.setText (str value)))))

(defn map-obj->om-element
  [factory op argtype argval]
  (let [xml-namespace (axis-op-namespace op)
        outer-element (make-om-elem factory xml-namespace (:name argtype))]
    
    (doseq [[key val] argval]
      (.addChild outer-element
                 (make-om-elem factory xml-namespace (name key) val)))
    outer-element))

(defn make-client [url options]
  (doto (org.apache.axis2.client.ServiceClient. nil (java.net.URL. url) nil nil)
    (.setOptions
     (let [opts (org.apache.axis2.client.Options.)]
        (.setTo opts (org.apache.axis2.addressing.EndpointReference. url))
        (when (options :timeout-millis)
          (.setTimeOutInMilliSeconds opts (options :timeout-millis))
          (.setProperty opts HTTPConstants/SO_TIMEOUT (options :timeout-millis))
          (.setProperty opts HTTPConstants/CONNECTION_TIMEOUT (options :timeout-millis)))
        (when (options :call-transport-cleanup)
          (.setCallTransportCleanup opts (options :call-transport-cleanup)))
        opts))))

(defn make-request [op & args]
  (let [factory (org.apache.axiom.om.OMAbstractFactory/getOMFactory)
        request (.createOMElement
                  factory (javax.xml.namespace.QName.
                            (axis-op-namespace op) (axis-op-name op)))
        op-args (axis-op-args op)]
    (doseq [[argval argtype] (map list args op-args)]
      
      (.addChild request
                 (if (nil? (:type argtype))
                   (map-obj->om-element factory op argtype argval)
                   (doto (.createOMElement
                           factory (javax.xml.namespace.QName. (axis-op-namespace op) (:name argtype)))
                     (.setText (obj->soap-str argval (:type argtype)))))))
    request))

(defn get-result [op retelem]
  (str retelem))

(defn client-call [client op & args]
  (locking client
    (if (isa? (class op) org.apache.axis2.description.OutOnlyAxisOperation)
     (.sendRobust client (.getName op) (apply make-request op args))
     (get-result
      op (.sendReceive client (.getName op) (apply make-request op args))))))

(defn client-proxy [url options]
  (let [client (make-client url options)]
    (->> (for [op (axis-service-operations (.getAxisService client))]
           [(keyword (axis-op-name op))
            {:fn (fn soap-call [& args]
                   (apply client-call client op args))
             :op op}])
         (into {}))))

(defn client-fn
  "Returns a SOAP client function, which is called as: (x :someMethod arg1 arg2 ...)
Options is a map currently supporting :timeout-millis and :call-transport-cleanup"
  ([url] (client-fn url {}))
  ([url options]
     (let [px (client-proxy url options)]
       (fn [opname & args]
         (cond
          (= opname :methods) (keys px)
          (= opname :sig) (:op (px (first args)))
          :else (apply (:fn (px opname)) args))))))
