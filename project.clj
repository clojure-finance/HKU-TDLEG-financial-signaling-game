(defproject financial-signaling-game "0.1.0-SNAPSHOT"
:description "Clojuring a financial signaling game"
:url "http://example.com/FIXME"
:license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
:dependencies [[org.clojure/clojure "1.10.3"]        
               [techascent/tech.ml.dataset "6.065"]
               [techascent/tech.ml "6.019"]
               [techascent/tech.viz "6.00-beta-16-2"]
               [scicloj/clojisr "1.0.0-BETA19"]
               [incanter "1.9.3"]]
:main financial-signaling-game.robust
:jvm-opts ["-Dclojure.tools.logging.factory=clojure.tools.logging.impl/jul-factory"]  
:repl-options {:init-ns financial-signaling-game.robust})
