(defproject forctional "0.1.0-SNAPSHOT"
  :aot :all
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.1"]]
  :java-source-paths ["src/forctional/jdbc"]
  :repl-options {:init-ns forctional.core})
