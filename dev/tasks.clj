(ns tasks
  (:require
   [clojure.string :as str]
   [com.biffweb.tasks :as biff-tasks]))

(defn hello
  "Says 'Hello'."
  []
  (println "Hello"))

(defn- gesso-source-root
  []
  (let [path-for
        (requiring-resolve
         'gesso.build.find-path/path-for)

        source-root
        (some-> (path-for "gesso")
                str
                (str/replace "\\" "/"))]
    (or source-root
        (throw
         (ex-info
          "Could not resolve the Gesso source root from the active classpath."
          {})))))

(defn css
  "Build CSS with Biff's normal Tailwind task while adding the Gesso source
   tree selected by the active Clojure classpath.

   Examples:

     clj -M:dev css
       Uses the pinned remote Gesso dependency.

     clj -M:local-gesso:dev css
       Uses ../gesso.

   Additional Tailwind arguments such as --watch and --minify are forwarded
   unchanged."
  [& args]
  (let [content
        (str/join
         ","
         ["./src/**/*"
          "./resources/**/*"
          (str (gesso-source-root) "/**/*")])]
    (apply
     biff-tasks/css
     (concat
      ["--content" content]
      args))))

;; Tasks should be vars (#'hello instead of hello) so that `clj -M:dev help`
;; can print their docstrings.
;;
;; Merging custom-tasks last intentionally replaces Biff's built-in "css"
;; task. Biff's existing dev, uberjar, deploy, and soft-deploy tasks all invoke
;; the task named "css", so they automatically use this override.
(def custom-tasks
  {"hello" #'hello
   "css" #'css})

(def tasks
  (merge biff-tasks/tasks
         custom-tasks))
