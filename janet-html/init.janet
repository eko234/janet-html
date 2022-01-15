#A *very* basic HTML generation library.
#Basic escaping features only, never use this on user input!

(def entity-replacements {"&" "&amp;" #must be first!
                          "<" "&lt;"
                          ">" "&gt;"
                          "\"" "&quot;"})

(def void-tags {:area true
                :base true
                :br true
                :col true
                :command true
                :embed true
                :hr true
                :img true
                :input true
                :keygen true
                :link true
                :meta true
                :param true
                :source true
                :track true
                :wbr true})

(defn void-tag? [tag-name]
  (void-tags tag-name))

(defn escape [s]
  (assert (= (type s) :string))
  (string/join
    (map |(if-let [character (string/from-bytes $)
                   replacement (entity-replacements character)]
            replacement
            character) s)))

(defn tag [tag-name attrs]
  (assert (or (= (type attrs) :table)
              (= (type attrs) :struct)) #NOTE is?
          (string "Missing attrs table: " tag-name))
  (let [attr-str (string/join (map (fn [[k v]]
                                     (if (= v true) k
                                       (string k "=\"" v "\""))) (pairs attrs)) " ")]
    (string "<" tag-name " " attr-str ">")))

(defn html [document &opt allow-no-escape?]
  (cond
    (= (type document) :string)
    (escape document)
    (and allow-no-escape? (= (document 1) :NO-ESCAPE))
    (document 2)
    (let [[tag-name attrs body] (apply (fn [first second & rest] [first second rest]) document)]
      (if (void-tag? tag-name)
        (tag tag-name attrs)
        (string (tag tag-name attrs)
                (string/join
                  (map |(html $ allow-no-escape?) body) " ")
                "</" tag-name ">")))))
