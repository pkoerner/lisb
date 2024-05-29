(ns lisb.prob.java-api
  (:import com.google.inject.Guice
           com.google.inject.Stage
           de.prob.MainModule
           de.prob.scripting.Api))

(def injector (Guice/createInjector Stage/PRODUCTION [(MainModule.)]))

(def api (.getInstance injector Api))

(defn state-space! [ast]
  (.b_load api ast))

