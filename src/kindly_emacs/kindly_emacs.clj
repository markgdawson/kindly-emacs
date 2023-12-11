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

(def kindly-plot nil)
(defmulti kindly-plot (fn [v & _args] (val->meta v)))

(defmethod kindly-plot :kind/vega-lite
  [val background-color]
  (->> (assoc val :background background-color)
       json/write-str
       darkstar/vega-lite-spec->svg
       ->svg-image))

(defmethod kindly-plot :kind/table
  [val background-color]
  (println "plot table...!"))


(comment
  (json/write-str a)

  (with-meta
    (assoc-in {:mark "bar",
               :encoding
               {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
                :y {:field "b", :type "quantitative"}}
               :width 500
               :height 44}
              [:data :values]
              [{:a "A", :b 28}
               {:a "B", :b 15}])
    {:kind/vega-lite true})





  (def data-for-plot [{:a "A", :b 28}
                      {:a "B", :b 55}])

  ^:kind/vega-lite
  {:data {:values data-for-plot}
   :mark "bar",
   :encoding
   {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
    :y {:field "b", :type "quantitative"}}
   :width 500}





  (val->meta (with-meta {:one 1} {:kind/vega-lite 1}))

  (val->meta val)

  (kindly-plot val)

  ;; (setq aaa (svg-image (file-to-string "/tmp/vg-example.svg")))
  ;; (setq-local iimage-mode-image-regex-alist '((";; kindly-image-file:\\[\\(.*\\)\\]" . 1)))

  ;; kindly-image-file:[/tmp/vg-example.svg]
  )
