(ns kindly-emacs.kindly-emacs
  (:require [applied-science.darkstar :as darkstar]
            [clojure.data.json :as json]))

(defn- val->meta [val]
  (->> val
       meta
       (filter (fn [[k v]] (and v (= "kind" (namespace k)))))
       keys
       first))

(defn- output-envelope [type content]
  {::type type
   :content content})

(defn- ->svg-image [svg-source]
  (output-envelope :svg-src svg-source))

(defmulti kindly-plot (fn [v & _args] (val->meta v)))

(defmethod kindly-plot :kind/vega-lite
  [val background-color]
  (->> (assoc val :background background-color)
       json/write-str
       darkstar/vega-lite-spec->svg
       ->svg-image))


(comment
  ;; Some plotting examples
  (with-meta
    (assoc-in {:mark "bar",
               :encoding
               {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
                :y {:field "b", :type "quantitative"}}
               :width 500
               :height 44
               :title "BBob"}
              [:data :values]
              [{:a "A", :b 28}
               {:a "D", :b 15}])
    {:kind/vega-lite true})


  (def data-for-plot [{:a "A", :b 28}
                      {:a "B", :b 55}
                      {:a "D", :b 15}])

  ^:kind/vega-lite
  {:data {:values data-for-plot}
   :mark "bar",
   :encoding
   {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
    :y {:field "b", :type "quantitative"}
    :color {:field :a}}
   :width 500}

  )
