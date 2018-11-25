(def project 'ga-labs)
(def version "0.1.0-SNAPSHOT")

(set-env! :source-paths   #{"src" "test"}
          :resource-paths #{"resources"}
          :dependencies   '[[org.clojure/clojure "1.9.0"]
                            [adzerk/boot-test "1.2.0" :scope "test"]
                            [onetom/boot-lein-generate "0.1.3" :scope "test"]])

(task-options!
 aot {:namespace   #{'ga-labs.core}}
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/yourname/ga-labs"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}}
 repl {:init-ns    'ga-labs.core}
 jar {:main        'ga-labs.core
      :file        (str "ga-labs-" version "-standalone.jar")})

; https://github.com/boot-clj/boot/wiki/For-Cursive-Users
(require 'boot.lein)
(boot.lein/generate)

(deftask build
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp (aot) (pom) (uber) (jar) (target :dir dir))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (with-pass-thru fs
    (require '[ga-labs.core :as app])
    (apply (resolve 'app/-main) args)))

(require '[adzerk.boot-test :refer [test]])
