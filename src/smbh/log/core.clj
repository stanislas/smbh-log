(ns ^{:author "Stanislas Nanchen"
      :doc    "smbh.log is a simple wrapper on logback (slf4j) and net.logstash.logback.

               To use the library, it is necessary to call the macro `deflogger`
               before any logging macro is called.

               It has some utilities to manipulate the MDC
               (`with`-style macro and ring middleware)."}
smbh.log.core
  (:require [cheshire.core :as json]
            [clojure.pprint :as pp]
            [clojure.string :as str])
  (:import [clojure.lang Keyword]
           [org.slf4j LoggerFactory Logger MDC]
           [smbh.log ClojureMapMarker]))

;; # Logger definition

;; Install a var called `⠇⠕⠶⠻` in the namespace from which it is called.
;; The name is logger in braille-2 notation."
(defmacro init-logger []
  `(if-not ~(resolve '⠇⠕⠶⠻)
     (defonce ~'⠇⠕⠶⠻ (LoggerFactory/getLogger ~(.toString *ns*)))))

;; # Logging macros
;;
;; In general, you should use the `info`, `warn`, etc variants as the `log` macros are considered low-level.
;;
;; For each log level, we have 3 macros with slight different arities and behavior:
;; 1. suffix `-c`:  3 arities
;;    i.   1 arg -> expects an exception created with `ex-info` and will use the data as context.
;;    ii.  2 args -> expects an exception created with `ex-info` and will use the data as context.
;;    iii. n args -> ctx, msg and arg : e is exception, if raise with `ex-info`, contributes to the ctx; msg with formatting
;; 2. suffix `-f`: formatting of message no context
;; 3. suffix `-e`: for exceptions with and without ctx works best with an exception created with `ex-info` : it will use
;;                 the data as context.
;;
;; The spy macro allows to log a value being evaluated (as well as the original expression) and return the evaluated
;; value. The first argument is the keyword of the level (:info, :warn, etc...)

(defmacro log-c
  ([method ctx]
   `(do (init-logger)
        (. ^Logger ~'⠇⠕⠶⠻
           (~method (ClojureMapMarker. ~ctx) ""))))
  ([method ctx msg]
   `(do (init-logger)
        (. ^Logger ~'⠇⠕⠶⠻
           (~method
             (ClojureMapMarker. ~ctx)
             ~msg))))
  ([method ctx msg & args]
   `(do (init-logger)
        ~(case (count args)
           0 `(. ^Logger ~'⠇⠕⠶⠻
                 (~method
                   (ClojureMapMarker. ~ctx)
                   ~msg))
           1 `(. ^Logger ~'⠇⠕⠶⠻
                 (~method
                   (ClojureMapMarker. ~ctx)
                   ~msg
                   ~(first args)))
           2 `(. ^Logger ~'⠇⠕⠶⠻
                 (~method
                   (ClojureMapMarker. ~ctx)
                   ~msg
                   ~(first args)
                   ~(second args)))
           `(. ^Logger ~'⠇⠕⠶⠻
               (~method
                 (ClojureMapMarker. ~ctx)
                 ~msg
                 (into-array [~@args])))))))

(defmacro log-m [method msg & args]
  `(do (init-logger)
       ~(case (count args)
          0 `(. ^Logger ~'⠇⠕⠶⠻
                (~method
                  ~msg))
          1 `(. ^Logger ~'⠇⠕⠶⠻
                (~method
                  ~msg
                  ~(first args)))
          2 `(. ^Logger ~'⠇⠕⠶⠻
                (~method
                  ~msg
                  ~(first args)
                  ~(second args)))
          `(. ^Logger ~'⠇⠕⠶⠻
              (~method
                ~msg
                (into-array [~@args]))))))

(defmacro log-e
  ([method e]
   `(let [e#   (cast Throwable ~e)
          ctx# (ex-data e#)
          msg# (.getMessage ^Exception e#)]
      (log-c ~method ctx# msg# e#)))
  ([method e msg]
   `(do (init-logger)
        (let [e#   (cast Throwable ~e)
              ctx# (ex-data e#)]
          (if ctx#
            (. ^Logger ~'⠇⠕⠶⠻
               (~method
                 (ClojureMapMarker. ctx#)
                 ~msg
                 ^Throwable ~e))
            (. ^Logger ~'⠇⠕⠶⠻
               (~method
                 ~msg
                 ^Throwable ~e))))))
  ([method e ctx msg]
   `(do (init-logger)
        (let [e#   (cast Throwable ~e)
              ctx# (ex-data e#)]
          (if ctx#
            (. ^Logger ~'⠇⠕⠶⠻
               (~method
                 (ClojureMapMarker. (into ctx# ~ctx))
                 ~msg
                 ^Throwable ~e))
            (. ^Logger ~'⠇⠕⠶⠻
               (~method
                 (ClojureMapMarker. ~ctx)
                 ~msg
                 ^Throwable ~e)))))))

(defmacro spy
  ([val]
   `(spy :debug ~val))
  ([level val]
   (let [method (symbol (name level))]
     `(let [val# ~val]
        (log-c ~method {:expression (delay
                                      (str/trim (with-out-str
                                                  (pp/with-pprint-dispatch
                                                    pp/code-dispatch
                                                    (pp/pprint '~val)))))
                        :value      val#}
               "spy")
        val#))))


(defmacro trace-c
  ([ctx]
   `(log-c ~'trace ~ctx))
  ([ctx msg]
   `(log-c ~'trace ~ctx ~msg))
  ([ctx msg e]
   `(log-c ~'trace ~ctx ~msg ~e)))

(defmacro trace-m [msg & args]
  `(log-m ~'trace ~msg ~@args))

(defmacro trace-e
  ([e]
   `(log-e ~'trace ~e))
  ([e msg]
   `(log-e ~'trace ~e ~msg))
  ([e ctx msg]
   `(log-e ~'trace ~e ~ctx ~msg)))


(defmacro debug-c
  ([ctx]
   `(log-c ~'debug ~ctx))
  ([ctx msg]
   `(log-c ~'debug ~ctx ~msg))
  ([ctx msg e]
   `(log-c ~'debug ~ctx ~msg ~e)))

(defmacro debug-m [msg & args]
  `(log-m ~'debug ~msg ~@args))

(defmacro debug-e
  ([e]
   `(log-e ~'debug ~e))
  ([e msg]
   `(log-e ~'debug ~e ~msg))
  ([e ctx msg]
   `(log-e ~'debug ~e ~ctx ~msg)))


(defmacro info-c
  ([ctx]
   `(log-c ~'info ~ctx))
  ([ctx msg]
   `(log-c ~'info ~ctx ~msg))
  ([ctx msg e]
   `(log-c ~'info ~ctx ~msg ~e)))

(defmacro info-m [msg & args]
  `(log-m ~'info ~msg ~@args))

(defmacro info-e
  ([e]
   `(log-e ~'info ~e))
  ([e msg]
   `(log-e ~'info ~e ~msg))
  ([e ctx msg]
   `(log-e ~'info ~e ~ctx ~msg)))


(defmacro warn-c
  ([ctx]
   `(log-c ~'warn ~ctx))
  ([ctx msg]
   `(log-c ~'warn ~ctx ~msg))
  ([ctx msg e]
   `(log-c ~'warn ~ctx ~msg ~e)))

(defmacro warn-m [msg & args]
  `(log-m ~'warn ~msg ~@args))

(defmacro warn-e
  ([e]
   `(log-e ~'warn ~e))
  ([e msg]
   `(log-e ~'warn ~e ~msg))
  ([e ctx msg]
   `(log-e ~'warn ~e ~ctx ~msg)))


(defmacro error-c
  ([ctx]
   `(log-c ~'error ~ctx))
  ([ctx msg]
   `(log-c ~'error ~ctx ~msg))
  ([ctx msg e]
   `(log-c ~'error ~ctx ~msg ~e)))

(defmacro error-m [msg & args]
  `(log-m ~'error ~msg ~@args))

(defmacro error-e
  ([e]
   `(log-e ~'error ~e))
  ([e msg]
   `(log-e ~'error ~e ~msg))
  ([e ctx msg]
   `(log-e ~'error ~e ~ctx ~msg)))

;; # MDC
;;
;; With the MDC, it is possible to register some key/values that are added to every log entry of the current thread.
;; (eg. user_id, request_id).
;;
;; Most usefull are the following functions/macros
;; - `with-mdc`, a macro to add some context (given as map) to the MDC, evaluate a body and remove the given context
;;               after execution of the body
;; - `wrap-mdc`, a ring middleware that takes a handler and a context creation function; this function is applied to
;;               the ring request and must return a context that will be added to the MDC for the duration of the
;;               handler.

(defn kw-str [^Keyword kw]
  (String/valueOf (.-sym kw)))

(defn mdc-copy []
  (or (MDC/getCopyOfContextMap) {}))

(defn mdc-set! [ctx-copy]
  (MDC/setContextMap ctx-copy))

(defn mdc-assoc! [key ^String value]
  (MDC/put (kw-str key) value))

(defn mdc-dissoc! [key]
  (MDC/remove (kw-str key)))

(defn mdc-get [key]
  (MDC/get (kw-str key)))

(defn mdc-assocs! [m val-fn]
  (persistent!
    (reduce-kv (fn [res k v]
                 (let [k2 (kw-str k)]
                   (MDC/put k2 (val-fn v))
                   (conj! res k2)))
               (transient [])
               m)))

(defn mdc-dissocs! [keys]
  (doseq [^String key keys]
    (MDC/remove key)))

(defmacro with-mdc [ctx val-fn & body]
  `(let [keys# (mdc-assocs! ~ctx ~val-fn)]
     (try
       ~@body
       (finally
         (mdc-dissocs! keys#)))))

(defmacro with-mdc-str [ctx & body]
  `(with-mdc ~ctx str ~@body))

(defn wrap-mdc [handler {:keys [ctx-fn val-fn] :or {val-fn str}}]
  (fn [request]
    (with-mdc (ctx-fn request)
              val-fn
              (handler request))))
