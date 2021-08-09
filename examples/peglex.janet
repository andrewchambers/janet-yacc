# This file implements an example of using peg to implement a lexer.
# See examples/calc.janet to see it in action.

(defn- wrap-rule
  [[kw peg]]
  [kw ~(cmt (* ($) (constant ,kw) (<- ,peg) ($)) ,|{:kind $1 :text $2 :span [$0 $3]})])

(defn rules-to-peg
  [rules]
  (def rules (map wrap-rule rules))
  (def grammar @{})
  (each [kw p] rules
    (put grammar kw p))
  (merge-into 
      grammar 
	  ~{:main (* (any :tok) (choice (not 1) :lex-error))
	    :tok ,(tuple 'choice ;(map first rules))
	    :lex-error (* (cmt ($) ,|{:kind :lex-error :span [$ $]}) 1)})
  (table/to-struct grammar))

(defn compile
  [rules]
  (def peg (rules-to-peg rules))
  (peg/compile peg))

(defn lex
  [lexer text &keys {:filter-ws filter-ws}]
  (default filter-ws true)
  (def tokens (peg/match lexer text))
  (if filter-ws
  	(filter |(not= :ws ($ :kind)) tokens)
    tokens))

