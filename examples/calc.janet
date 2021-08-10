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
     (expr (:num) ,|(int/s64 ($0 :text))
           (expr :+ expr) ,|(+ $0 $2)
           (expr :- expr) ,|(- $0 $2)
           (expr :* expr) ,|(* $0 $2)
           (expr :/ expr) ,|(/ $0 $2)
           (:- expr) ,|(- $1)
           (:lparen expr :rparen) ,(fn [_ $1 _] $1))))

(def parser-tables (yacc/compile calculator-grammar))

(defn eval
  [prog]
  (def tokens (peglex/lex lexer prog))
  (match (yacc/parse parser-tables tokens)
    [:syntax-error tok nil]
    (errorf "syntax error: unexpected end of file after token '%s' ending at col %d"
            (string (tok :kind)) ((tok :span) 1))
    [:syntax-error _ tok]
    (errorf "syntax error: unexpected token '%s' at col %d"
            (string (tok :kind)) ((tok :span) 0))
    [:ok result]  result))

(defn repl
  []
  (while true
    (if-let [buf (getline)
             prog (string buf)
             result (eval prog)]
      (print (string result))
      (break))))

(defn main
  [&]
  (when (os/getenv "YYDEBUG")
    (setdyn :yydebug stderr))
  (repl))
