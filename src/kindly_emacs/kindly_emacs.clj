(ns kindly-emacs.kindly-emacs
  (:require [applied-science.darkstar :as darkstar]
            [clojure.data.json :as json]))

(json/write-str a)

(defn- val->meta [val]
  (->> val
       meta
       (filter (fn [[k v]] (and v (= "kind" (namespace k)))))
       keys
       first))

(defn ->image [image-src]
  {:image image-src})

(comment
  (def val (with-meta
             (assoc-in {:mark "bar",
                        :encoding
                        {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
                         :y {:field "b", :type "quantitative"}}}
                       [:data :values]
                       [{:a "A", :b 28}
                        {:a "B", :b 55}])
             {:kind/vega-lite true}))
  (val->meta val)
  (->> val
       json/write-str
       darkstar/vega-lite-spec->svg
       (spit "/tmp/vg-example.svg")
       )

  )

;; `file://tmp/vg-example.svg'



(val->meta (with-meta {:one 1} {:kind/vega-lite 1}))

(defmulti kindly-plot val->meta)

(defmethod kindly-plot :kind/vega-lite
  [val]
  (println val)
  (println "plot vega lite...!")
  (->image "image-path..."))

(defmethod kindly-plot :kind/table
  [val]
  (println "plot table...!"))

(kindly-plot a)
