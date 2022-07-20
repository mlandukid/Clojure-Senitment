(ns navigator.sentiment.gcp.core
  (:import (com.google.cloud.language.v1beta2 LanguageServiceClient
                                              Document
                                              Document$Type
                                              AnalyzeSentimentResponse
                                              Sentiment
                                              EncodingType)))

(defn sentiment->map [s]
  (let [magnitude         (.getMagnitude s)
        score             (.getScore s)
        magnitude-x-score (* magnitude score)]

    {:magnitude         magnitude
     :score             score
     :magnitude-x-score magnitude-x-score}))


(defn sentence->map [s]
  {:text      (-> (.getText s)
                  (.getContent))
   :sentiment (-> (.getSentiment s)
                  (sentiment->map))})

(defn mention->map [m]
  {:begin-offset (-> (.getText m)
                     (.getBeginOffset))
   :content      (-> (.getText m)
                     (.getContent))
   :sentiment    (-> (.getSentiment m)
                     sentiment->map)
   :type         (-> (.getType m)
                     (.toString))})

(defn entity->map [e]
  {:name      (.getName e)
   :sentiment (-> (.getSentiment e)
                  (sentiment->map))
   :saliance  (.getSalience e)
   :type      (-> (.getType e)
                  (.toString))
   :mentions  (map mention->map (seq (.getMentionsList e)))})


(defn analyze-sentiment [text]
  (let [client    (LanguageServiceClient/create)
        doc       (-> (Document/newBuilder)
                      (.setContent text)
                      (.setType Document$Type/PLAIN_TEXT)
                      (.build))
        response  (.analyzeSentiment client doc EncodingType/UTF16)
        sentiment (.getDocumentSentiment response)]

    {:document  {:score (.getScore sentiment) :magnitude (.getMagnitude sentiment)}
     :sentences (mapv sentence->map (seq (.getSentencesList response)))}))


(defn analyze-entity-sentiment [text]
  (let [client   (LanguageServiceClient/create)
        doc      (-> (Document/newBuilder)
                     (.setContent text)
                     (.setType Document$Type/PLAIN_TEXT)
                     (.build))
        response (.analyzeEntitySentiment client doc EncodingType/UTF16)]

    {:entities (map entity->map (seq (.getEntitiesList response)))}))


(defn overall-text-analysis [text]
  (let [entities (analyze-entity-sentiment text)
        sentences+overall (analyze-sentiment text)]

    (-> {}
        (assoc-in [:company-call/sentiment-results :entities]
                  (:entities entities))
        (assoc-in [:company-call/sentiment-results :sentences]
                  (:sentences sentences+overall)))))
