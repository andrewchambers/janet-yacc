(import ../yacc)

# Define a lexer for our calculator using janet peg.

(defn make-number-tok [text]
  {:kind :num :val (scan-number text)})

(defn make-op-tok [text]
  {:kind (keyword text)})

(def lexer-peg
  ~{:main (some (choice :number :op))
    :number (cmt (capture (some (range "09"))) ,make-number-tok)
    :op (cmt (capture (choice "*" "+" "-")) ,|{:kind (keyword $)})})

(defn lex
  [text]
  (peg/match lexer-peg text))

# Define our  

(def calculator-grammar
  ~(yacc
     (%left :+ :-)
     (%nonassoc :*)
     (expr (:num) ,|($0 :val)
           (expr :+ expr) ,|(+ $0 $2)
           (expr :- expr) ,|(- $0 $2)
           (expr :* expr) ,|(* $0 $2))))

(def prog "1+2*3-4")

(def tokens (lex prog))

# Uncomment to see debug state.
# (setdyn :yydbg stderr)

(def result
  (match (yacc/parse calculator-grammar tokens)
    [:syntax-error _] (error "syntax error")
    [:ok result] result))

(printf "%s is %d" prog result)
