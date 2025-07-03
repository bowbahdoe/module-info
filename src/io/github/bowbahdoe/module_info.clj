(ns io.github.bowbahdoe.module-info
  (:import (java.io InputStream)
           (java.lang.classfile ClassBuilder ClassFile ClassModel)
           (java.lang.classfile.attribute ModuleAttribute
                                          ModuleAttribute$ModuleAttributeBuilder
                                          ModuleExportInfo
                                          ModuleHashInfo
                                          ModuleHashesAttribute
                                          ModuleMainClassAttribute
                                          ModulePackagesAttribute
                                          ModuleProvideInfo
                                          ModuleRequireInfo
                                          ModuleTargetAttribute)
           (java.lang.classfile.constantpool ClassEntry ModuleEntry PackageEntry Utf8Entry)
           (java.lang.constant ClassDesc ModuleDesc PackageDesc)
           (java.lang.reflect AccessFlag)
           (java.nio.file CopyOption Path Files StandardCopyOption)
           (java.nio.file.attribute FileAttribute)
           (java.util Collection Enumeration HexFormat List Optional Set)
           (java.util.jar Attributes Attributes$Name JarEntry JarFile JarOutputStream Manifest)
           (java.util.zip ZipFile))
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(set! *warn-on-reflection* true)

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
               (merge info
                      (cond->
                        {:name (-> (ModuleAttribute/.moduleName attribute)
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
                                                     (as-> class-symbol
                                                           (str (ClassDesc/.packageName class-symbol)
                                                                "."
                                                                (ClassDesc/.displayName class-symbol))))})
                                     (ModuleAttribute/.uses attribute))}
                        (Optional/.isPresent (ModuleAttribute/.moduleVersion attribute))
                        (assoc :version (-> (ModuleAttribute/.moduleVersion attribute)
                                            (Optional/.map (fn [v]
                                                             (Utf8Entry/.stringValue v)))
                                            (Optional/.orElse nil)))

                        (-> (ModuleAttribute/.moduleFlags attribute)
                            (Set/.contains AccessFlag/OPEN))
                        (assoc :open true)

                        (-> (ModuleAttribute/.moduleFlags attribute)
                            (Set/.contains AccessFlag/SYNTHETIC))
                        (assoc :synthetic true)

                        (-> (ModuleAttribute/.moduleFlags attribute)
                            (Set/.contains AccessFlag/MANDATED))
                        (assoc :mandated true)))


               ;; Technically optional, so we will skip it
               (instance? ModulePackagesAttribute attribute)
               (assoc info :packages
                           (->> (ModulePackagesAttribute/.packages attribute)
                                (mapv (fn [package]
                                        {:package
                                         (PackageDesc/.name
                                           (PackageEntry/.asSymbol package))}))))

               ;; Ignore for a second...
               (instance? ModuleHashesAttribute attribute)
               (assoc info :hashes
                           {:algorithm (-> (ModuleHashesAttribute/.algorithm attribute)
                                           (Utf8Entry/.stringValue))
                            :hashes    (->> (ModuleHashesAttribute/.hashes attribute)
                                            (mapv (fn [hash]
                                                    {:module (-> (ModuleHashInfo/.moduleName hash)
                                                                 (ModuleEntry/.asSymbol)
                                                                 (ModuleDesc/.name))
                                                     :hash        (->> (ModuleHashInfo/.hash hash)
                                                                      (HexFormat/.formatHex
                                                                        (HexFormat/of)))})))})

               (instance? ModuleMainClassAttribute attribute)
               (assoc info :main-class
                           (-> (ModuleMainClassAttribute/.mainClass attribute)
                               (ClassEntry/.asSymbol)
                               (as-> _ (str (ClassDesc/.packageName _)
                                            "."
                                            (ClassDesc/.displayName _)))))

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
        module-info' (update module-info :requires
                             (fn [requires]
                               (reduce
                                 (fn [new-requires require]
                                   (if (= (:module require) "java.base")
                                     (conj new-requires (update require assoc :mandated true))
                                     (conj new-requires require)))
                                 []
                                 requires)))
        ;; Second, if there is no java.base require, add one
        module-info'' (if-not (some #(= (:module %) "java.base") (:requires module-info'))
                        (update module-info' :requires conj {:module "java.base" :mandated true})
                        module-info')

        ;; But all of that is moot if its actually java.base we are producing
        module-info (if (not= (:name module-info) "java.base")
                      module-info''
                      module-info)]
    (-> (ClassFile/of)
        (ClassFile/.buildModule
          (ModuleAttribute/of
            (ModuleDesc/of (:name module-info))
            (fn [module-attribute-builder]
              (when (:version module-info)
                (ModuleAttribute$ModuleAttributeBuilder/.moduleVersion
                  module-attribute-builder
                  (:version module-info)))
              (let [access-flags (cond-> []
                                         (:open module-info)
                                         (conj AccessFlag/OPEN)
                                         (:synthetic module-info)
                                         (conj AccessFlag/SYNTHETIC)
                                         (:mandated module-info)
                                         (conj AccessFlag/MANDATED))]
                (when (seq access-flags)
                  (^[AccessFlag/1]
                    ModuleAttribute$ModuleAttributeBuilder/.moduleFlags
                    module-attribute-builder
                    (into-array AccessFlag access-flags))))
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
            (when-let [packages (:packages module-info)]
              (ClassBuilder/.accept
                class-builder
                (^[List]
                  ModulePackagesAttribute/ofNames
                  (mapv #(PackageDesc/of (:package %)) packages))))
            (when-let [main-class (:main-class module-info)]
              (ClassBuilder/.accept
                class-builder
                (ModuleMainClassAttribute/of (ClassDesc/of main-class))))
            (when-let [target-platform (:target-platform module-info)]
              (ClassBuilder/.accept
                class-builder
                (^[String] ModuleTargetAttribute/of target-platform)))
            (when-let [hashes (:hashes module-info)]
              (ClassBuilder/.accept
                class-builder
                (^[String List]
                  ModuleHashesAttribute/of
                  (:algorithm hashes)
                  (mapv (fn [{:keys [module hash]}]
                          (ModuleHashInfo/of
                            (ModuleDesc/of module)
                            (-> (HexFormat/of)
                                (HexFormat/.parseHex hash))))
                        (:hashes hashes)))))
            (ClassBuilder/.withVersion
              class-builder
              ClassFile/JAVA_9_VERSION
              0))))))

(defn from-jar
  [jar]
  (let [jar-file (if (instance? JarFile jar)
                   jar
                   (JarFile. ^String jar))
        entries (JarFile/.entries jar-file)]
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

(defn from-jmod
  [jmod]
  (let [zip-file (if (instance? ZipFile jmod)
                   jmod
                   (ZipFile. ^String jmod))
        entries (ZipFile/.entries zip-file)]
    (println  zip-file)
    (loop [module-entries []]
     (if (.hasMoreElements entries)
       (let [entry (.nextElement entries)]
         (if (or (= (JarEntry/.getName entry) "classes/module-info.class")
                 (re-matches #"classes/META-INF/versions/([0-9]+)/module-info.class"
                             (JarEntry/.getName entry)))
           (recur (conj module-entries entry))
           (recur module-entries)))
       (when-let [entry-to-use (or (first (filter #(= (JarEntry/.getName %) "module-info.class")
                                                  module-entries))
                                   (first (sort-by #(JarEntry/.getName %) module-entries)))]
         (with-open [is (ZipFile/.getInputStream zip-file entry-to-use)]
           (from-bytes (InputStream/.readAllBytes is))))))))


(defn enrich-jar
  "module-info can either be the map or a string path to an edn file"
  [{:keys [in-path out-path module-info]}]
  (if (or (nil? out-path)
          (= in-path out-path))
    (let [temp-out (Files/createTempFile "out" ".jar" (into-array FileAttribute []))]
      (try
        (enrich-jar {:in-path in-path
                     :out-path (str temp-out)
                     :module-info module-info})
        (^[Path Path CopyOption/1]
          Files/copy
          temp-out
          (Path/of in-path (into-array String []))
          (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
        (finally
          (Files/deleteIfExists temp-out))))

    (let [module-info (if (string? module-info)
                        (edn/read-string (slurp module-info))
                        module-info)
          module-info-bytes (to-bytes module-info)
          jar (JarFile. ^String in-path)
          jar-entries (JarFile/.entries jar)
          manifest    (JarFile/.getManifest jar)]
      (Attributes/.put (Manifest/.getMainAttributes manifest)
                       (Attributes$Name. "Multi-Release")
                       "true")
      (with-open [out (JarOutputStream. (io/output-stream out-path) manifest)]
        (loop []
          (when (Enumeration/.hasMoreElements jar-entries)
            (let [e (Enumeration/.nextElement jar-entries)]
              ;; We copy the manifest above
              (when-not (= (JarEntry/.getName e)
                           "META-INF/MANIFEST.MF")
                (.putNextEntry out e)
                (when-not (and (JarEntry/.isDirectory e))
                  (with-open [content (JarFile/.getInputStream jar e)]
                    (InputStream/.transferTo content out)))
                (JarOutputStream/.closeEntry out)))
            (recur)))
        (let [module-info-entry (JarEntry. "META-INF/versions/9/module-info.class")]
          (JarOutputStream/.putNextEntry out module-info-entry)
          (^[byte/1] JarOutputStream/.write out module-info-bytes)
          (JarOutputStream/.closeEntry out))))))

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
                                  (packages-in-jar "test-modules/clojure.jar"))}})



  (from-bytes
    (to-bytes {:name "org.clojure"
               :requires [{:module "java.xml"}
                          {:module "java.desktop"}
                          {:module "jdk.unsupported"}]
               :exports [{:package "clojure"}
                         {:package "clojure.asm"}
                         {:package "clojure.core"}
                         {:package "clojure.data"}]
               :main-class "a.apple"})))
(comment
  (from-jar "test-modules/org.jspecify.jar")
  (from-jar "test-modules/dev.mccue.json.jar")
  (from-jar "test-modules/org.slf4j.jar")
  (from-jar "test-modules/org.slf4j.simple.jar")
  (from-jar "test-modules/dev.mccue.guava.primitives.jar"))

(comment
  (from-bytes
    (Files/readAllBytes
      (Path/of "module-info.class" (make-array String 0)))))

(defn packages-in-jar
  [jar]
  (let [jar-file (if (instance? JarFile jar)
                   jar
                   (JarFile. ^String jar))
        entries (JarFile/.entries jar-file)]
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

(comment
  (packages-in-jar "jmods/java.base.jmod"))

(comment
  (enrich-jar
    {:in-path "test-modules/clojure.jar"
     :out-path "clojure-moduled.jar"
     :module-info {:name "org.clojure"
                   :requires [{:module "java.xml"}
                              {:module "java.desktop"}
                              {:module "jdk.unsupported"}]
                   :exports (mapv (fn [package]
                                    {:package package})
                                  (packages-in-jar "test-modules/clojure.jar"))}}))