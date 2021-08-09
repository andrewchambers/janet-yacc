(import yacc)
(import ./peglex)

(def lexer-grammar
  ~[[:ws :s+] # Automatically filtered by peglex
    [:+ "+"]
    [:- "-"]
    [:* "*"]
    [:/ "/"]
    [:lparen "("]
    [:rparen ")"]
    [:num :d+]])

(def lexer (peglex/compile lexer-grammar))

(def calculator-grammar
  ~(yacc
     (%left :+ :-)
     (%left :* :/)
     (prog () _
           (expr) ,|$0)
     (expr (:num) ,|(scan-number ($0 :text))
           (expr :+ expr) ,|(+ $0 $2)
           (expr :- expr) ,|(- $0 $2)
           (expr :* expr) ,|(* $0 $2)
           (expr :/ expr) ,|(/ $0 $2)
           (:lparen expr :rparen) ,(fn [_ $1 _] $1))))

(def parser (yacc/compile calculator-grammar))

(defn eval
  [prog]
  (def tokens (peglex/lex lexer prog))
  (def result
    (match (yacc/parse parser tokens)
      [:syntax-error tok] (errorf "syntax error: unexpected token '%s' at col %d"
                                  (string (tok :kind)) ((tok :span) 0))
      [:ok result] result)))

(defn repl
  []
  (while true
    (if-let [buf (getline)
             prog (string buf)
             result (eval prog)]
      (printf "%d" result)
      (break))))

(defn main
  [&]
  (repl))
