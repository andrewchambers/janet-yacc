(import ../yacc)

# Test program for original miniyacc.
#
# %token num lparen rparen
# %left add sub
# %nonassoc mul
# %nonassoc umin
# 
# %%
# 
# s:
#  | s a
#  ;
# 
# a: num              
#  | a add a
#  | a sub a
#  | a mul a
#  | sub a %prec umin
#  | lparen a rparen
#  ;

(def grammar
  ~(yacc
     (%token :num :lparen :rparen)
     (%left :add :sub)
     (%nonassoc :mul)
     (%nonassoc :umin)
     (s
       () _
       (s a) _)
     (a
       (:num) _
       (a :add a) _
       (a :sub a) _
       (a :mul a) _
       (sub a %prec :umin) _
       (:lparen a :rparen) _)))

(def dbg-output @"")

(setdyn :yydbg dbg-output)

(def tabs 
  (with-dyns [:dbg-output dbg-output]
    (yacc/compile grammar)))

# (pp tabs)

(assert (= (tabs :yyr1) [
  2   0   2   1   3   3   3   2   3
]))

(assert (= (tabs :yyr2) [
  0   1   1   2   2   2   2   2   2
]))

(assert (= (tabs :yyadef) [
   1  -1   0   2   3   4   5   6   7  -1
  -1  -1  -1  -1  -1   8
]))

(assert (= (tabs :yygdef)  [
 -1   1   3
]))

# (pp (tabs :yyadsp))
#(assert (= (tabs :yyadsp)  [
#  -8   0  -8   6  -8  -3  -3  -2  -8   3
#  12  12  12  12  12  -8
#]))

(assert (= (tabs :yygdsp)  [
 -16 -16   8
]))

#(pp (tabs :yyact))
#(assert (= (tabs :yyact)  [
#   2   4  14  12  -1  13  15  10  11  12
#  10  11  12   4  14  -1  -1  13   5   6
#   7   8   9
#]))

#(assert (= (tabs :yychk)  [
#   0   1   2   6   6   5   3   4   5   6
#   4   5   6   1   2  -1  -1   5  10  10
#  10  10  10
#]))