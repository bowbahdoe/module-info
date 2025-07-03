help:
    just --list

get_test_modules:
    jresolve \
      --output-directory test-modules \
      --use-module-names \
      --purge-output-directory \
      pkg:maven/dev.mccue/json@2024.11.20 \
      pkg:maven/org.slf4j/slf4j-simple@2.0.17 \
      pkg:maven/dev.mccue/guava-primitives@33.4.0 \
      pkg:maven/org.clojure/clojure@1.12.0

jar:
    rm -rf build
    jar --create --file build/jar/io.github.bowbahdoe.module-info.jar -C src .
    clojure -X \
      "io.github.bowbahdoe.module-info/enrich-jar" \
      '{:in-path "build/jar/moduleinfo.jar" :module-info "module-info.edn"}'