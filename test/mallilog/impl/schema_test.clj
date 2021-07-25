(ns mallilog.impl.schema-test
  (:require
   [mallilog.impl.schema :as sut]
   [clojure.test :as t]
   [malli.core :as m]))

(defn validate
  [schema value]
  (m/validate [:schema {:registry sut/-relational-schemas} schema] value))

(t/deftest wildcard
  (t/is (validate :wildcard '_)))

(t/deftest variable
  (t/testing "Variable is a simple symbol starting with ?"
    (t/is (validate :variable '?foo)))
  (t/testing "Namespaced symbols cannot be variables"
    (t/is (not (validate :variable '?foo/bar))))
  (t/testing "Symbol must start with ?"
    (t/is (not (validate :variable 'bar)))))

(t/deftest plain-symbol
  (t/testing "simple symbol"
    (t/is (validate :plain-symbol 'bar))
    (t/is (not (validate :plain-symbol 'foo/bar))))
  (t/testing "Cannot begin with ? or $"
    (t/is (not (validate :plain-symbol '$bar)))
    (t/is (not (validate :plain-symbol '?bar)))))

(t/deftest ea-pattern
  (t/is (validate :ea-pattern '[?e 1]))
  (t/is (validate :ea-pattern '[?e :k]))
  (t/is (validate :ea-pattern '[?e ?a]))
  (t/is (validate :ea-pattern '[?e [?a ...]]))
  (t/is (validate :ea-pattern '[?e [?a ... ?as]]))
  (t/is (not (validate :ea-pattern '[?e 1 2])))
  (t/is (not (validate :ea-pattern '[?e [?a ... as]]))))
