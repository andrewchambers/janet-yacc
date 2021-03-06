(import _yacc)

(defn compile
  `
  Precompile a yacc grammar into the runtime tables.
  `
  [grammar]
  (unless (= (grammar 0) 'yacc)
    (errorf "%m is not a valid grammar" grammar))
  (_yacc/compile ;(slice grammar 1)))

(defmacro- yydebugf [& args]
  ~(when yydebug (with-dyns [:err (dyn :yydebug)] ,(tuple 'eprintf ;args))))

(defn parse
  `
  Parse a function using a given yacc grammar
  or precompiled yacc grammar tables.

  toks can be a function, a generator, an array or tuple
  of tokens. Each token must have a :kind field which must
  be a keyword.

  returns:

  [:ok result]
  [:syntax-error cur-tok lookahead-tok]

  lookahead-tok may be nil at eof.
  `
  [tab toks]

  (def yydebug (not (nil? (dyn :yydebug))))
  (def yyini (tab :yyini))
  (def yyntoks (tab :yyntoks))
  (def yyact (tab :yyact))
  (def yyfns (tab :yyfns))
  (def yyadef (tab :yyadef))
  (def yygdef (tab :yygdef))
  (def yyadsp (tab :yyadsp))
  (def yygdsp (tab :yygdsp))
  (def yytrns (tab :yytrns))
  (def yychk (tab :yychk))
  (def yyr1 (tab :yyr1))
  (def yyr2 (tab :yyr2))

  (var r 0)
  (var h 0)
  (var n 0)
  (var s yyini)
  (var tk -1)
  (var yyval nil)
  (var yylval nil)

  (def stk @[@{:state yyini :val yyval}])

  (defn syntax-error
    []
    (return :yyparse [:syntax-error yylval]))

  (def yylex
    (cond
      (indexed? toks)
      (do
        (var idx -1)
        (fn yylex [] (get toks (++ idx))))
      (fiber? toks)
      (fn yylex [] (resume toks))
      toks))

  (var do-reduce nil)
  (var do-stack nil)

  (defn do-loop
    []

    (set n (yyadsp s))
    (when (and (< tk 0)
               (> n (- yyntoks)))
      (set yylval (yylex))
      (if yylval
        (set tk
             (or (yytrns (yylval :kind))
                 (syntax-error)))
        (set tk 0)))
    (set n (+ n tk))
    (if (or (< n 0)
            (>= n (length yyact))
            (not= (yychk n) tk))
      (do
        (set r (yyadef s))
        (when (< r 0)
          (syntax-error))
        (do-reduce))
      (do
        (set n (yyact n))
        (when (= n -1)
          (syntax-error))
        (if (< n 0)
          (do
            (set r (- (+ n 2)))
            (do-reduce))
          (do
            (set tk -1)
            (set yyval yylval)
            (do-stack))))))

  (set do-stack
       (fn
         []
         (set s n)
         (array/push stk @{:state s :val yyval})
         (yydebugf "shift: %p" yyval)
         (do-loop)))

  (set do-reduce
       (fn
         []
         (def args @[])
         (repeat (yyr1 r)
           (array/push args ((array/pop stk) :val)))
         (reverse! args)
         (set h (yyr2 r))
         (set s ((last stk) :state))
         (set n (+ (yygdsp h) s))

         (if (or (< n 0)
                 (>= n (length yyact))
                 (not= (yychk n) (+ yyntoks h)))
           (set n (yygdef h))
           (set n (yyact n)))

         (def action (get yyfns r))
         (yydebugf "reduce: %p" args)
         (cond
           (= action :done)
           [:ok (args 0)]
           (do
             (set yyval (and action (action ;args)))
             (do-stack)))))

  (prompt :yyparse
          (do-loop)))
