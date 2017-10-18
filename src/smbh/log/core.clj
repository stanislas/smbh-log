(ns ^{:author "Stanislas Nanchen"
      :doc    "smbh.log is a simple wrapper on logback (slf4j) and net.logstash.logback.

               To use the library, it is necessary to call the macro `deflogger` before any logging macro is called.

               It has some utilities to manipulate the MDC (`with`-style macro and ring middleware)."}
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
(defmacro deflogger []
  `(defonce ~'⠇⠕⠶⠻ (LoggerFactory/getLogger ~(.toString *ns*))))

;; # Logging macros
;;
;; In general, you should use the `info`, `warn`, etc variants as the `log` macros are considered low-level.
;;
;; For each log level, we have 4 macros with slight different arities and behavior:
;; 1. no-suffix: most common, 3 arities
;;    i.   1 arg -> expects an exception created with `ex-info` and will use the data as context.
;;    ii.  2 args -> ctx and msg : ctx is a map of keywords to values and is serialized as marker.
;;    iii. 3 args -> ctx, msg and e : e is exception, if raise with `ex-info`, contributes to the ctx.
;; 2. suffix `-cf`: ctx and formatting of message
;; 3. suffix `-f`: formatting of message
;; 4. suffix `-e`: for general exceptions (without context).
;;
;; The spy macro allows to log a value being evaluated (as well as the original expression) and return the evaluated
;; value. The first argument is the keyword of the level (:info, :warn, etc...)

(defmacro log
  ([method e]
   `(let [e#   (cast Throwable ~e)
          ctx# (ex-data e#)
          msg# (.getMessage ^Exception e#)]
      (log ~method ctx# msg# e#)))
  ([method ctx msg]
   (if (resolve '⠇⠕⠶⠻)
     `(. ^Logger ~'⠇⠕⠶⠻
         (~method
           (ClojureMapMarker. ~ctx)
           ~msg))
     (throw (IllegalStateException. "(deflogger) has not been called"))))
  ([method ctx msg e]
   (if (resolve '⠇⠕⠶⠻)
     `(let [e#   (cast Throwable ~e)
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
               ^Throwable ~e))))
     (throw (IllegalStateException. "(deflogger) has not been called")))))

(defmacro log-cf [method ctx msg & args]
  (if (resolve '⠇⠕⠶⠻)
    (case (count args)
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
            (into-array [~@args]))))
    (throw (IllegalStateException. "(deflogger) has not been called"))))

(defmacro log-f [method msg & args]
  (if (resolve '⠇⠕⠶⠻)
    (case (count args)
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
            (into-array [~@args]))))
    (throw (IllegalStateException. "(deflogger) has not been called"))))

(defmacro log-e [method msg e]
  (if (resolve '⠇⠕⠶⠻)
    `(let [e#   (cast Throwable ~e)
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
              ^Throwable ~e))))
    (throw (IllegalStateException. "(deflogger) has not been called"))))

(defmacro spy
  ([val]
   `(spy :debug ~val))
  ([level val]
   (let [method (symbol (name level))]
     `(let [val# ~val]
        (log ~method {:expression (delay
                                    (str/trim (with-out-str
                                                (pp/with-pprint-dispatch
                                                  pp/code-dispatch
                                                  (pp/pprint '~val)))))
                      :value      val#}
             "spy")
        val#))))

(defmacro trace
  ([e]
   `(log ~'trace ~e))
  ([ctx msg]
   `(log ~'trace ~ctx ~msg))
  ([ctx msg e]
   `(log ~'trace ~ctx ~msg ~e)))

(defmacro trace-cf [ctx msg & args]
  `(log-cf ~'trace ~ctx ~msg ~@args))

(defmacro trace-f [msg & args]
  `(log-f ~'trace ~msg ~@args))

(defmacro trace-e
  ([e]
   `(trace ~e))
  ([msg e]
   `(log-e ~'trace ~msg ~e)))

(defmacro debug
  ([e]
   `(log ~'debug ~e))
  ([ctx msg]
   `(log ~'debug ~ctx ~msg))
  ([ctx msg e]
   `(log ~'debug ~ctx ~msg ~e)))

(defmacro debug-cf [ctx msg & args]
  `(log-cf ~'debug ~ctx ~msg ~@args))

(defmacro debug-f [msg & args]
  `(log-f ~'debug ~msg ~@args))

(defmacro debug-e
  ([e]
   `(debug ~e))
  ([msg e]
   `(log-e ~'debug ~msg ~e)))

(defmacro info
  ([e]
   `(log ~'info ~e))
  ([ctx msg]
   `(log ~'info ~ctx ~msg))
  ([ctx msg e]
   `(log ~'info ~ctx ~msg ~e)))

(defmacro info-cf [ctx msg & args]
  `(log-cf ~'info ~ctx ~msg ~@args))

(defmacro info-f [msg & args]
  `(log-f ~'info ~msg ~@args))

(defmacro info-e
  ([e]
   `(info ~e))
  ([msg e]
   `(log-e ~'info ~msg ~e)))

(defmacro warn
  ([e]
   `(log ~'warn ~e))
  ([ctx msg]
   `(log ~'warn ~ctx ~msg))
  ([ctx msg e]
   `(log ~'warn ~ctx ~msg ~e)))

(defmacro warn-cf [ctx msg & args]
  `(log-cf ~'warn ~ctx ~msg ~@args))

(defmacro warn-f [msg & args]
  `(log-f ~'warn ~msg ~@args))

(defmacro warn-e
  ([e]
   `(warn ~e))
  ([msg e]
   `(log-e ~'warn ~msg ~e)))

(defmacro error
  ([e]
   `(log ~'error ~e))
  ([ctx msg]
   `(log ~'error ~ctx ~msg))
  ([ctx msg e]
   `(log ~'error ~ctx ~msg ~e)))

(defmacro error-cf [ctx msg & args]
  `(log-cf ~'error ~ctx ~msg ~@args))

(defmacro error-f [msg & args]
  `(log-f ~'error ~msg ~@args))

(defmacro error-e
  ([e]
   `(error ~e))
  ([msg e]
   `(log-e ~'error ~msg ~e)))


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

(defn mdc-assoc! [key value]
  (MDC/put (kw-str key) (json/encode value)))

(defn mdc-dissoc! [key]
  (MDC/remove (kw-str key)))

(defn mdc-get [key]
  (MDC/get (kw-str key)))

(defn mdc-assocs! [m]
  (persistent!
    (reduce-kv (fn [res k v]
                 (let [k2 (kw-str k)]
                   (MDC/put k2 (json/encode v))
                   (conj! res k2)))
               (transient [])
               m)))

(defn mdc-dissocs! [keys]
  (doseq [^String key keys]
    (MDC/remove key)))

(defmacro with-mdc [ctx & body]
  `(let [keys# (mdc-assocs! ~ctx)]
     (try
       ~@body
       (finally
         (mdc-dissocs! keys#)))))

(defn wrap-mdc [handler ctx-fn]
  (fn [request]
    (with-mdc (ctx-fn request)
              (handler request))))
