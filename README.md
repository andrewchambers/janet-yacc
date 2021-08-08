# janet-yacc

An implementation of yacc for the janet programming language.

The implementation is based heavily on https://c9x.me/yacc/ .

Example:
```
(import yacc)

...

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

# Optionally precompile.
#(def yacc-tables
#  (yacc/compile calculator-grammar))

(def result
  (match (yacc/parse calculator-grammar tokens)
    [:syntax-error _] (error "syntax error")
    [:ok result] result))

(printf "%s is %d" prog result)

```

Help with documentation and tests wanted :).


Todo:

- Function to convert parser state to an error message of some form.