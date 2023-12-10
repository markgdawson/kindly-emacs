(ns test-kindly
  (:require  [clojure.test :as t]))

(with-meta
  (assoc-in {:mark "bar",
             :encoding
             {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
              :y {:field "b", :type "quantitative"}}}
            [:data :values]
            [{:a "A", :b 28}
             {:a "B", :b 55}])
  {:kind/vega-lite true})

^:kind/table
[{:a "1" :b "2"} {:a "3" :b "4"}]

(let [v (with-meta (assoc-in {:mark "bar",
                              :encoding
                              {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
                               :y {:field "b", :type "quantitative"}}}
                             [:data :values]
                             [{:a "A", :b 28}
                              {:a "B", :b 55}])
          {:kind/meta true})]
  (meta v))

(let [v ^:kind/meta {:mark "bar",
         :encoding
         {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
          :y {:field "b", :type "quantitative"}}}]
  (meta v))


;;;

(let [val (with-meta
            (assoc-in {:mark "bar",
                       :encoding
                       {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
                        :y {:field "b", :type "quantitative"}}}
                      [:data :values]
                      [{:a "A", :b 28}
                       {:a "B", :b 55}])
            {:kind/vega-lite true})] {:meta (->> val meta (filter (fn [[k v]] (and v (= "kind" (namespace k)))))) :value val})(let [val (with-meta
            (assoc-in {:mark "bar",
                       :encoding
                       {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
                        :y {:field "b", :type "quantitative"}}}
                      [:data :values]
                      [{:a "A", :b 28}
                       {:a "B", :b 55}])
            {:kind/vega-lite true})] {:meta (->> val meta (filter (fn [[k v]] (and v (= "kind" (namespace k))))) keys first) :value val})




(reset! bob/value 1)

@bob/value
