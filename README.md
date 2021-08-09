# janet-yacc

An implementation of yacc for the janet programming language.

The implementation is based heavily on https://c9x.me/yacc/.

Example from ./examples/calc.janet:
```
(import yacc)

...

(def calculator-grammar
  ~(yacc
     (%left :+ :-)
     (%nonassoc :*)
     (expr (:num) ,|(scan-number ($0 :text))
           (expr :+ expr) ,|(+ $0 $2)
           (expr :- expr) ,|(- $0 $2)
           (expr :* expr) ,|(* $0 $2)
           (:lparen expr :rparen) ,(fn [_ $1 _] $1))))

(def parser (yacc/compile calculator-grammar))

(yacc/parse tokens)
```

Todo:

- Some sort of runtime debug tracing.
