(ns dev.mccue.module-info
  (:import (java.io InputStream)
           (java.lang.classfile ClassBuilder ClassFile ClassModel)
           (java.lang.classfile.attribute ModuleAttribute ModuleAttribute$ModuleAttributeBuilder ModuleExportInfo ModuleHashInfo ModuleHashesAttribute ModuleMainClassAttribute ModulePackagesAttribute ModuleProvideInfo ModuleRequireInfo ModuleTargetAttribute)
           (java.lang.classfile.constantpool ClassEntry ModuleEntry PackageEntry Utf8Entry)
           (java.lang.constant ClassDesc ModuleDesc PackageDesc)
           (java.lang.reflect AccessFlag)
           (java.nio.file Path Files)
           (java.util Collection HexFormat List Map Optional Set)
           (java.util.jar Attributes Attributes$Name JarEntry JarFile JarOutputStream Manifest))
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn from-bytes
  "Reads the bytes of a module-info.class file into a map."
  [^bytes bytes]
  (let [class-model (-> (ClassFile/of)
                        (ClassFile/.parse bytes))]
    (when-not (ClassModel/.isModuleInfo class-model)
      (throw (IllegalArgumentException. "Class file does not represent a module.")))
    (->> (ClassModel/.attributes class-model)
         (List/.stream)
         (stream-reduce!
           (fn [info attribute]
             (cond
               (instance? ModuleAttribute attribute)
               (merge info {:name (-> (ModuleAttribute/.moduleName attribute)
                                      (ModuleEntry/.name)
                                      (Utf8Entry/.stringValue))

                            :exports (mapv (fn [export]
                                             (cond->
                                               {:package (-> (ModuleExportInfo/.exportedPackage export)
                                                             (PackageEntry/.asSymbol)
                                                             (PackageDesc/.name))}

                                               (not (List/.isEmpty (ModuleExportInfo/.exportsTo export)))
                                               (assoc :to (->> (ModuleExportInfo/.exportsTo export)
                                                               (mapv (fn [module-entry]
                                                                       (-> (ModuleEntry/.asSymbol module-entry)
                                                                           (ModuleDesc/.name))))))

                                               (-> (ModuleExportInfo/.exportsFlags export)
                                                   (Set/.contains AccessFlag/SYNTHETIC))
                                               (assoc :synthetic true)

                                               (-> (ModuleExportInfo/.exportsFlags export)
                                                   (Set/.contains AccessFlag/MANDATED))
                                               (assoc :mandated true)))


                                           (ModuleAttribute/.exports attribute))
                            :requires (mapv (fn [require]
                                              (cond->
                                                {:module (->> (ModuleRequireInfo/.requires require)
                                                              (ModuleEntry/.asSymbol)
                                                              (ModuleDesc/.name))}

                                                (Optional/.isPresent (ModuleRequireInfo/.requiresVersion require))
                                                (assoc :version (-> (ModuleRequireInfo/.requiresVersion require)
                                                                    (Optional/.map (fn [version-desc]
                                                                                     (-> version-desc
                                                                                         (Utf8Entry/.stringValue))))
                                                                    (Optional/.orElse nil)))

                                                (-> (ModuleRequireInfo/.requiresFlags require)
                                                    (Set/.contains AccessFlag/STATIC_PHASE))
                                                (assoc :static true)

                                                (-> (ModuleRequireInfo/.requiresFlags require)
                                                    (Set/.contains AccessFlag/TRANSITIVE))
                                                (assoc :transitive true)

                                                (-> (ModuleRequireInfo/.requiresFlags require)
                                                    (Set/.contains AccessFlag/MANDATED))
                                                (assoc :mandated true)

                                                (-> (ModuleRequireInfo/.requiresFlags require)
                                                    (Set/.contains AccessFlag/SYNTHETIC))
                                                (assoc :synthetic true)))
                                            (ModuleAttribute/.requires attribute))
                            :provides (mapv (fn [provides]
                                              {:service (-> (ModuleProvideInfo/.provides provides)
                                                            (ClassEntry/.asSymbol)
                                                            (as-> _ (str (ClassDesc/.packageName _)
                                                                         "."
                                                                         (ClassDesc/.displayName _))))
                                               :with    (mapv (fn [class-entry]
                                                                (-> class-entry
                                                                    (ClassEntry/.asSymbol)
                                                                    (as-> _ (str (ClassDesc/.packageName _)
                                                                                 "."
                                                                                 (ClassDesc/.displayName _)))))
                                                              (ModuleProvideInfo/.providesWith provides))})
                                            (ModuleAttribute/.provides attribute))
                            :uses (mapv (fn [use]
                                          {:service (-> use
                                                        (ClassEntry/.asSymbol)
                                                        (as-> class-symbol (str (ClassDesc/.packageName class-symbol)
                                                                                "."
                                                                                (ClassDesc/.displayName class-symbol))))})
                                        (ModuleAttribute/.uses attribute))})

               ;; Technically optional, so we will skip it
               #_(instance? ModulePackagesAttribute attribute)
               #_(assoc info :packages
                             (->> (ModulePackagesAttribute/.packages attribute)
                                  (mapv (fn [package]
                                          (PackageDesc/.name
                                            (PackageEntry/.asSymbol package))))))

               ;; Ignore for a second...
               #_(instance? ModuleHashesAttribute attribute)
               #_(assoc info :hashes
                             {:algorithm (-> (ModuleHashesAttribute/.algorithm attribute)
                                             (Utf8Entry/.stringValue))
                              :hashes    (-> (ModuleHashesAttribute/.hashes attribute)
                                             (mapv (fn [hash]
                                                     {:module-name (ModuleHashInfo/.moduleName hash)
                                                      :hash        (->> (ModuleHashInfo/.hash hash)
                                                                        (HexFormat/.formatHex
                                                                          (HexFormat/of)))})))})

               (instance? ModuleMainClassAttribute attribute)
               (assoc info :main-class
                           (-> (ModuleMainClassAttribute/.mainClass attribute)
                               (ClassEntry/.name)
                               (Utf8Entry/.stringValue)))

               (instance? ModuleTargetAttribute attribute)
               (assoc info :target-platform
                           (-> (ModuleTargetAttribute/.targetPlatform attribute)
                               (Utf8Entry/.stringValue)))

               :else
               info))
           {}))))

(defn to-bytes
  [module-info]
  (let [;; First, if there is a java.base require, make sure it is mandated
        module-info (update module-info :requires
                            (fn [requires]
                              (reduce
                                (fn [new-requires require]
                                  (if (= (:module require) "java.base")
                                    (conj new-requires (update require assoc :mandated true))
                                    (conj new-requires require)))
                                []
                                requires)))
        ;; Second, if there is no java.base require, add one
        module-info (if-not (some #(= (:module %) "java.base") (:requires module-info))
                      (update module-info :requires conj {:module "java.base" :mandated true})
                      module-info)]
    (-> (ClassFile/of)
        (ClassFile/.buildModule
          (ModuleAttribute/of
            (ModuleDesc/of (:name module-info))
            (fn [module-attribute-builder]
              (doseq [export (:exports module-info)]
                (^[PackageDesc Collection ModuleDesc/1]
                  ModuleAttribute$ModuleAttributeBuilder/.exports
                  module-attribute-builder
                  (PackageDesc/of (:package export))
                  (cond-> []
                          (:synthetic export)
                          (conj AccessFlag/SYNTHETIC)
                          (:mandated export)
                          (conj AccessFlag/MANDATED))
                  (into-array ModuleDesc (map #(ModuleDesc/of %)
                                              (:to export)))))
              (doseq [require (:requires module-info)]
                (^[ModuleDesc Collection String]
                  ModuleAttribute$ModuleAttributeBuilder/.requires
                  module-attribute-builder
                  (ModuleDesc/of (:module require))
                  (cond-> []
                          (:static require)
                          (conj AccessFlag/STATIC_PHASE)
                          (:transitive require)
                          (conj AccessFlag/TRANSITIVE)
                          (:synthetic require)
                          (conj AccessFlag/SYNTHETIC)
                          (:mandated require)
                          (conj AccessFlag/MANDATED))
                  (:version require)))
              (doseq [provide (:provides module-info)]
                (ModuleAttribute$ModuleAttributeBuilder/.provides
                  module-attribute-builder
                  (^[ClassDesc ClassDesc/1]
                    ModuleProvideInfo/of
                    (ClassDesc/of (:service provide))
                    (->> (:with provide)
                         (map #(ClassDesc/of %))
                         (into-array ClassDesc)))))
              (doseq [use (:uses module-info)]
                (ModuleAttribute$ModuleAttributeBuilder/.uses
                  module-attribute-builder
                  (ClassDesc/of (:service use))))))
          (fn [class-builder]
            (when-let [main-class (:main-class module-info)]
              (ClassBuilder/.accept
                class-builder
                (ModuleMainClassAttribute/of (ClassDesc/of main-class))))
            (when-let [target-platform (:target-platform module-info)]
              (ClassBuilder/.accept
                class-builder
                (^[String] ModuleTargetAttribute/of target-platform)))
            (ClassBuilder/.withVersion
              class-builder
              ClassFile/JAVA_9_VERSION
              0))))))

(defn from-jar
  [jar]
  (let [jar-file (if (instance? JarFile jar)
                   jar
                   (JarFile. ^String jar))
        entries (.entries jar-file)]
    (loop [module-entries []]
      (if (.hasMoreElements entries)
        (let [entry (.nextElement entries)]
          (if (or (= (JarEntry/.getName entry) "module-info.class")
                  (re-matches #"META-INF/versions/([0-9]+)/module-info.class"
                              (JarEntry/.getName entry)))
            (recur (conj module-entries entry))
            (recur module-entries)))
        (when-let [entry-to-use (or (first (filter #(= (JarEntry/.getName %) "module-info.class")
                                                   module-entries))
                                    (first (sort-by #(JarEntry/.getName %) module-entries)))]
          (with-open [is (JarFile/.getInputStream jar-file entry-to-use)]
            (from-bytes (InputStream/.readAllBytes is))))))))

(defn enrich-jar
  [{:keys [in-path out-path module-info]}]
  (let [module-info-bytes (to-bytes module-info)
        jar (JarFile. ^String in-path)
        jar-entries (JarFile/.entries jar)
        manifest    (JarFile/.getManifest jar)]
    (Attributes/.put (Manifest/.getMainAttributes manifest)
                     (Attributes$Name. "Multi-Release")
                     "true")
    (with-open [out (JarOutputStream. (io/output-stream out-path) manifest)]
      (loop []
        (when (.hasMoreElements jar-entries)
          (let [e (.nextElement jar-entries)]
            ;; We copy the manifest above
            (when-not (= (JarEntry/.getName e)
                         "META-INF/MANIFEST.MF")
              (.putNextEntry out e)
              (when-not (and (JarEntry/.isDirectory e))
                (with-open [content (JarFile/.getInputStream jar e)]
                  (.transferTo content out)))
              (JarOutputStream/.closeEntry out)))
          (recur)))
      (let [module-info-entry (JarEntry. "META-INF/versions/9/module-info.class")]
        (.putNextEntry out module-info-entry)
        (^[byte/1] JarOutputStream/.write out module-info-bytes)
        (JarOutputStream/.closeEntry out)))))

(comment
  (enrich-jar
    {:in-path "test-modules/clojure.jar"
     :out-path "clojure-moduled.jar"
     :module-info {:name "org.clojure"
                   :requires [{:module "java.xml"}
                              {:module "java.desktop"}
                              {:module "jdk.unsupported"}]
                   :exports (mapv (fn [pkg]
                                    {:package pkg})
                                  (packages-in-jar "test-modules/clojure.jar"))}}))

(comment
  (enrich-jar
    {:in-path "test-modules/clojure.jar"
     :out-path "clojure-moduled.jar"
     :module-info {:name "org.clojure"
                   :requires [{:module "java.xml"}
                              {:module "java.desktop"}
                              {:module "jdk.unsupported"}]
                   :exports [{:package "clojure"}
                             {:package "clojure.asm"}
                             {:package "clojure.core"}
                             {:package "clojure.data"}]}})

  (from-bytes
    (to-bytes {:name "org.clojure"
               :requires [{:module "java.xml"}
                          {:module "java.desktop"}
                          {:module "jdk.unsupported"}]
               :exports [{:package "clojure"}
                         {:package "clojure.asm"}
                         {:package "clojure.core"}
                         {:package "clojure.data"}]
               :main-class "apple"})))
(comment
  (from-jar "test-modules/org.jspecify.jar")
  (from-jar "test-modules/dev.mccue.json.jar")
  (from-jar "test-modules/org.slf4j.jar")
  (from-jar "test-modules/org.slf4j.simple.jar")
  (from-jar "test-modules/dev.mccue.guava.primitives.jar"))

;; a instanceof ModuleAttribute
;             || a instanceof ModulePackagesAttribute
;             || a instanceof ModuleHashesAttribute
;             || a instanceof ModuleMainClassAttribute
;             || a instanceof ModuleResolutionAttribute
;             || a instanceof ModuleTargetAttribute
;             || a instanceof InnerClassesAttribute
;             || a instanceof SourceFileAttribute
;             || a instanceof SourceDebugExtensionAttribute
;             || a instanceof RuntimeVisibleAnnotationsAttribute
;             || a instanceof RuntimeInvisibleAnnotationsAttribute
;             || a instanceof CustomAttribute
(comment
  (parse-bytes
    (Files/readAllBytes
      (Path/of "module-info.class" (make-array String 0)))))

(defn packages-in-jar
  [jar]
  (let [jar-file (if (instance? JarFile jar)
                   jar
                   (JarFile. ^String jar))
        entries (.entries jar-file)]
    (loop [packages #{}]
      (if (.hasMoreElements entries)
        (let [entry (.nextElement entries)]
          (if (string/ends-with? (JarEntry/.getName entry) ".class")
            (let [package-name (with-open [is (JarFile/.getInputStream jar-file entry)]
                                 (-> (ClassFile/of)
                                     (ClassFile/.parse (InputStream/.readAllBytes is))
                                     (ClassModel/.thisClass)
                                     (ClassEntry/.asSymbol)
                                     (ClassDesc/.packageName)))]
              (recur (conj packages package-name)))
            (recur packages)))
        packages))))

(defn modules-used-by-jar
  [jar]
  (let [jar-file (if (instance? JarFile jar))
                 jar
                 (JarFile. ^String jar)
        entries (.entries jar-file)]
    (loop [modules #{}]
      (if (.hasMoreElements entries)
        (let [entry (.nextElement entries)]
          (if (string/ends-with? (JarEntry/.getName entry) ".class")
            (let [package-name (with-open [is (JarFile/.getInputStream jar-file entry)]
                                 (-> (ClassFile/of)
                                     (ClassFile/.parse (InputStream/.readAllBytes is))
                                     (ClassModel/.thisClass)
                                     (ClassEntry/.asSymbol)
                                     (ClassDesc/.packageName)))]
              (recur (conj modules package-name)))
            (recur modules)))
        modules))))

(comment
  (packages-in-jar "test-modules/clojure.jar"))