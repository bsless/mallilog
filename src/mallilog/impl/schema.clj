(ns mallilog.impl.schema
  (:require
   [clojure.string :as str]
   [malli.core :as m]
   [malli.error :as me]))

(defn -wildcard-schema [] [:= '_])

(defn -fn-arg-schema
  "fn-arg = (variable | constant | src-var)"
  []
  [:orn
   [:variable :variable]
   [:constant ::constant]
   [:src-var :src-var]])

;;; with-clause                = ':with' variable+
;;; where-clauses              = ':where' clause+
;;; inputs                     = ':in' (src-var | binding | pattern-name | rules-var)+


(defn src-var-symbol?
  [s]
  (str/starts-with? (name s) "$"))

(defn -src-var-schema
  "src-var = symbol starting with $"
  []
  (m/-simple-schema
   {:type :variable,
    :pred #(and (simple-symbol? %) (src-var-symbol? %))}))

(defn variable-symbol?
  [s]
  (str/starts-with? (name s) "?"))

(defn -variable-schema
  "variable = symbol starting with ?"
  []
  (m/-simple-schema
   {:type :variable,
    :pred (fn [s] (and (simple-symbol? s)
                      (variable-symbol? s)))}))

;;; rules-var         = the symbol "%"

(defn -plain-symbol-schema
  "plain-symbol = symbol that does not begin with $ or ?"
  []
  [:and
   simple-symbol?
   [:not :src-var]
   [:not :variable]])

;;; pattern-name      = plain-symbol

(defn -and-clause-schema
  "and-clause = [ 'and' clause+ ]"
  []
  [:cat
   [:= 'and]
   [:+ [:schema [:ref ::clause]]]])

(defn -expression-clause-schema
  "expression-clause = (data-pattern | pred-expr | fn-expr | rule-expr)"
  []
  [:orn
   [:pred-expr :pred-expr]
   [:fn-expr :fn-expr]
   [:ea-pattern :ea-pattern] ; my addition
   [:eav-pattern :eav-pattern] ; my addition
   [:rule-expr :rule-expr]
   [:data-pattern :data-pattern]])

(defn -rule-expr-schema
  "rule-expr = [ src-var? rule-name (variable | constant | '_')+]"
  []
  [:catn
   [:src-var [:? :src-var]]
   [:rule-name :symbol]
   [:+ [:altn
        [:variable :variable]
        [:constant ::constant]
        [:wildcard :wildcard]]]])

(defn -not-clause-schema
  "not-clause = [ src-var? 'not' clause+ ]"
  []
  [:catn
   [:src [:? :src-var]]
   [:not [:= 'not]]
   [:clauses [:+ [:schema [:ref ::clause]]]]])

;;; not-join-clause   = [ src-var? 'not-join' [variable+] clause+ ] TODO

(defn -or-clause-schema
  "or-clause = [ src-var? 'or' (clause | and-clause)+]"
  []
  [:catn
   [:src [:? :src-var]]
   [:or [:= 'or]]
   [:clauses [:+ [:alt
                  [:schema [:ref ::clause]]
                  [:schema [:ref ::and-clause]]]]]])

;;; or-join-clause    = [ src-var? 'or-join' rule-vars (clause | and-clause)+ ] TODO

(defn -rule-vars-schema
  "rule-vars = [variable+ | ([variable+] variable*)]"
  []
  [:alt
   [:+ :variable]
   [:cat [:sequential {:min 1} :variable] [:* :variable]]])

(defn -clause-schema
  "clause = (not-clause | not-join-clause | or-clause | or-join-clause | expression-clause)
  NOTE: Not all clauses are implemented at the moment."
  []
  [:orn
   [:not-clause [:schema [:ref ::not-clause]]]
   [:or-clause [:schema [:ref ::or-clause]]]
   [:expression-clause :expression-clause]])

(defn -coll-data-pattern-schema
  "[variable ... variable?]"
  []
  [:catn
   [:variable :variable]
   [:_ [:= '...]]
   [:as [:? :variable]]])

(defn -pattern-element-schema
  []
  [:altn
   [:variable :variable]
   [:constant ::constant]
   [:wildcard :wildcard]
   [:coll [:schema :coll-data-pattern]]])

(defn -ea-pattern-schema
  []
  [:catn
   [:e :pattern-element]
   [:a :pattern-element]])

(defn -eav-pattern-schema
  []
  [:catn
   [:e :pattern-element]
   [:a :pattern-element]
   [:v :pattern-element]])

(defn -data-pattern-schema
  "data-pattern = [ src-var? (variable | constant | '_')+ ]"
  []
  [:catn
   [:src-var [:? :src-var]]
   [:pattern [:+ [:altn
                  [:variable :variable]
                  [:constant ::constant]
                  [:wildcard :wildcard]]]]])

(defn -constant-schema
  "constant = any non-variable data literal"
  []
  [:or
   :int
   :double
   :string
   :keyword
   [:and :symbol [:not :variable] [:not [:= '...]]]
   [:and [:sequential [:schema [:ref ::constant]]]]
   [:map-of [:schema [:ref ::constant]] [:schema [:ref ::constant]]]]
  #_[:and :any [:not :variable]])

(defn -pred-expr-schema
  "pred-expr = [ [pred fn-arg+] ]"
  []
  [:catn
   [:expr [:schema
           [:catn
            [:pred :pred]
            [:args [:+ :fn-arg]]]]]])

(def preds '#{= == <= >= < > contains? starts-with?})

(defn -pred-schema
  []
  (into [:enum] preds))

(defn -fn-expr-schema
  "fn-expr = [ [fn fn-arg+] binding]"
  []
  [:catn
   [:expr [:schema [:catn
                    [:function :function]
                    [:args [:+ :fn-arg]]]]]
   [:binding :binding]])

(def functions '#{+ - * / quot subs})

(defn -function-schema [] (into [:enum] functions))

(defn -binding-schema
  "binding = (bind-scalar | bind-tuple | bind-coll | bind-rel)"
  []
  [:orn
   [:bind-scalar :bind-scalar]
   [:bind-tuple :bind-tuple]
   [:bind-coll :bind-coll]
   [:bind-rel :bind-rel]])

(defn -bind-scalar-schema
  "bind-scalar = variable"
  []
  :variable)

(defn -bind-tuple-schema
  "bind-tuple = [ (variable | '_')+]"
  []
  [:cat [:+ [:alt :variable :wildcard]]])

(defn -bind-coll-schema
  "bind-coll = [variable '...']"
  []
  [:cat :variable [:= '...]])

(defn -bind-rel-schema
  "bind-rel          = [ [(variable | '_')+] ]"
  []
  [:cat
   [:schema
    [:cat
     [:+
      [:altn
       [:variable :variable]
       [:wildcard :wildcard]]]]]])


(defn relational-schemas
  []
  {:wildcard          (-wildcard-schema)
   :_                 (-wildcard-schema)
   :src-var           (-src-var-schema)
   :variable          (-variable-schema)
   :plain-symbol      (-plain-symbol-schema)
   ::and-clause       (-and-clause-schema)
   :expression-clause (-expression-clause-schema)
   :rule-expr         (-rule-expr-schema)
   ::not-clause       (-not-clause-schema)
   ::or-clause        (-or-clause-schema)
   :rule-vars         (-rule-vars-schema)
   ::clause           (-clause-schema)
   :coll-data-pattern (-coll-data-pattern-schema)
   :pattern-element   (-pattern-element-schema)
   :ea-pattern        (-ea-pattern-schema)
   :eav-pattern       (-eav-pattern-schema)
   :data-pattern      (-data-pattern-schema)
   ::constant         (-constant-schema)
   :pred-expr         (-pred-expr-schema)
   :pred              (-pred-schema)
   :fn-expr           (-fn-expr-schema)
   :fn-arg            (-fn-arg-schema)
   :function          (-function-schema)
   :binding           (-binding-schema)
   :bind-scalar       (-bind-scalar-schema)
   :bind-tuple        (-bind-tuple-schema)
   :bind-coll         (-bind-coll-schema)
   :bind-rel          (-bind-rel-schema)})

(def -relational-schemas (relational-schemas))

(def schema (m/schema [:schema {:registry -relational-schemas} [:+ ::clause]]))
(def validator (m/validator schema))
(def parser (m/parser schema))
(def explainer (m/explainer schema))

(comment

  (parser '([?e [?a ... ?as]]))
  (me/humanize
   (m/explain [:schema {:registry -relational-schemas} [:cat :variable [:schema :coll-data-pattern]]] '[?e [?a ... ?as]]))
  (validator '([?e :x ?x]))
  (parser '([?e :x ?x]))
  (parser '([?e :x [?x ...]]))
  (parser '[[?e :x ?x]
            [?e :y ?y]
            [(< ?x ?y)]
            [(+ ?x ?y) ?v]]))
