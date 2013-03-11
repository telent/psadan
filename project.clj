;;; apt-get install libunixsocket-java

(defproject psadan "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :resource-paths ["/usr/share/java/unix-0.5.jar"]
  :url "http://example.com/FIXME"
  :jvm-opts ["-Djava.library.path=native/:/usr/lib/jni/"]
  :main psadan.core
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/data.zip "0.1.1"]
                 ])



