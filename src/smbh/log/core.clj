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
(defmacro deflogger []
  `(defonce ~'⠇⠕⠶⠻ (LoggerFactory/getLogger ~(.toString *ns*))))

;; # Logging macros
;;
;; In general, you should use the `info`, `warn`, etc variants 
;; as the `log` macros are considered low-level.
;;
;; For each log level, we have 3 macros with slight different arities and behavior:
;; 1. suffix `-c`:  3 arities
;;    i.   1 arg -> expects an exception created with `ex-info` and will use the data as context.
;;    ii.  2 args -> expects an exception created with `ex-info` and will use the data as context.
;;    iii. n args -> ctx, msg and arg : e is exception, if raise with `ex-info`, 
;;                   contributes to the ctx; msg with formatting
;; 2. suffix `-m`: formatting of message no context
;; 3. suffix `-e`: for exceptions with and without ctx works best with an exception 
;;                 created with `ex-info` : it will use the data as context.
;;
;; The spy macro allows to log a value being evaluated (as well as the original expression) 
;; and return the evaluated value. The first argument is the keyword of the level 
;; (:info, :warn, etc...)

(defmacro log-c
  ([method ctx]
   (if (resolve '⠇⠕⠶⠻)
     `(. ^Logger ~'⠇⠕⠶⠻
         (~method (ClojureMapMarker. ~ctx) ""))
     (throw (IllegalStateException. "(deflogger) has not been called"))))
  ([method ctx msg]
   (if (resolve '⠇⠕⠶⠻)
     `(. ^Logger ~'⠇⠕⠶⠻
         (~method
           (ClojureMapMarker. ~ctx)
           ~msg))
     (throw (IllegalStateException. "(deflogger) has not been called"))))
  ([method ctx msg & args]
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
             (into-array Object [~@args]))))
     (throw (IllegalStateException. "(deflogger) has not been called")))))

(defmacro log-m [method msg & args]
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
            (into-array Object [~@args]))))
    (throw (IllegalStateException. "(deflogger) has not been called"))))

(defmacro log-e
  ([method e]
   `(let [e#   (cast Throwable ~e)
          ctx# (ex-data e#)
          msg# (.getMessage ^Exception e#)]
      (log-c ~method ctx# msg# e#)))
  ([method e msg]
   (if (resolve '⠇⠕⠶⠻)
     `(let [e#     (cast Throwable ~e)
            e-ctx# (ex-data e#)]
        (if e-ctx#
          (. ^Logger ~'⠇⠕⠶⠻
             (~method
               (ClojureMapMarker. e-ctx#)
               ~msg
               ^Throwable e#))
          (. ^Logger ~'⠇⠕⠶⠻
             (~method
               ~msg
               ^Throwable e#))))
     (throw (IllegalStateException. "(deflogger) has not been called"))))
  ([method e ctx msg]
   (if (resolve '⠇⠕⠶⠻)
     `(let [e#     (cast Throwable ~e)
            e-ctx# (ex-data e#)
            ctx#   ~ctx]
        (if e-ctx#
          (. ^Logger ~'⠇⠕⠶⠻
             (~method
               (ClojureMapMarker. (into e-ctx# ctx#))
               ~msg
               ^Throwable e#))
          (. ^Logger ~'⠇⠕⠶⠻
             (~method
               (ClojureMapMarker. ctx#)
               ~msg
               ^Throwable e#))))
     (throw (IllegalStateException. "(deflogger) has not been called")))))

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
  ([ctx msg & args]
   `(log-c ~'trace ~ctx ~msg ~@args)))

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
  ([ctx msg & args]
   `(log-c ~'debug ~ctx ~msg ~@args)))

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
  ([ctx msg & args]
   `(log-c ~'info ~ctx ~msg ~@args)))

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
  ([ctx msg & args]
   `(log-c ~'warn ~ctx ~msg ~@args)))

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
  ([ctx msg & args]
   `(log-c ~'error ~ctx ~msg ~@args)))

(defmacro error-m [msg & args]
  `(log-m ~'error ~msg ~@args))

(defmacro error-e
  ([e]
   `(log-e ~'error ~e))
  ([e msg]
   `(log-e ~'error ~e ~msg))
  ([e ctx msg]
   `(log-e ~'error ~e ~ctx ~msg)))
