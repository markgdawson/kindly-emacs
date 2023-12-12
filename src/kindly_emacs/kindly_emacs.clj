(ns kindly-emacs.kindly-emacs
  (:require
   [clojure.data.json :as json]
   [clojure.pprint :as pprint]
   [kindly-emacs.darkstar :as darkstar]
   [scicloj.kindly-advice.v1.api :refer [advise]]
   [tablecloth.api :as tc]))

(defn- val->meta [val]
  (:kind (advise {:value val})))

(defn- output-envelope [type content]
  {::type type
   :content content})

(defn- ->svg-image [svg-source]
  (output-envelope :svg-src svg-source))

(defn- ->text [content]
  (output-envelope :text content))

(defmulti kindly-plot (fn [v & _args] (val->meta v)))

(defmethod kindly-plot :kind/vega-lite
  [val background-color]
  (->> (assoc val :background background-color)
       json/write-str
       darkstar/vega-lite-spec->svg
       ->svg-image))

(defn dataset-rows->string [dataset header?]
  (binding [*out* (java.io.StringWriter.)]
    (pprint/print-table (tc/rows dataset :as-maps))
    (let [s (.toString *out*)]
      (if header?
        s
        (-> s
            clojure.string/split-lines
            (nthnext 3)
            (->> (clojure.string/join "\n")
                 (str "\n")))))))

(defmethod kindly-plot :kind/dataset
  [val _bg-color]
  (let [row-count (tc/row-count val)]
    (->text (format "\nRows: %d\n%s"
                    row-count
                    (if (<= row-count 25)
                      (dataset-rows->string val true)
                      (str (-> val
                               (tc/head 7)
                               (dataset-rows->string true))
                           "..."
                           (-> val
                               (tc/head 7)
                               (dataset-rows->string false))))))))

(comment

  ;; Some plotting examples
  (with-meta
    (assoc-in {:mark "bar",
               :encoding
               {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
                :y {:field "b", :type "quantitative"}}
               :width 500
               :height 44
               :title "Title"}
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
