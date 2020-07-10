(ns capacity.validation)

(def constant-val-reg
  {:message "Constants values must all be numbers"
   :finder #(map last (:constants %))
   :validator (partial every? number?)})

(defn validation-error
  [form messages]
  (throw (RuntimeException. (apply str
                                   (concat (str "Validation errors:\n")
                                           (interleave
                                            (map #(str (inc %) ". ")
                                                 (range))
                                            messages
                                            (repeat "\n"))
                                           (str "\nFrom form \n" form))))))

(defn valid-by?
  [validation form]
  (let [{validator :validator
         finder :finder} validation]
    (-> form finder validator)))

(defn- collect-validation-messages
  [form error-messages validation]
  (if (valid-by? validation form)
    error-messages
    (conj error-messages (:message validation))))

(defn validation-messages
  [form validations]
  (reduce (partial collect-validation-messages form)
          []
          validations))

(defn validate
  [form & validations]
  (let [messages (validation-messages form validations)]
    (if (empty? messages)
      form
      (validation-error form messages))))
