(ns capacity.validation)

(defn validation-error
  "Throws a RuntimeException with the given messages."
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
  "Runs the `validation` (finder and validator) on the `form`."
  [validation form]
  (let [{validator :validator
         finder :finder} validation]
    (-> form finder validator)))

(defn- collect-validation-messages
  "Passed to reduce to collect messages that don't pass validation."
  [form error-messages validation]
  (if (valid-by? validation form)
    error-messages
    (conj error-messages (:message validation))))

(defn validation-messages
  "Returns all messages from validations that don't pass."
  [form validations]
  (reduce (partial collect-validation-messages form)
          []
          validations))

(defn validate
  "Runs `validations` on `form`

  If they all pass then returns the form, otherwise throws
  a RuntimeException with all messages from failing validations."
  [form & validations]
  (let [messages (validation-messages form validations)]
    (if (empty? messages)
      form
      (validation-error form messages))))
