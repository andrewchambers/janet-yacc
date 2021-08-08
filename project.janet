(declare-project
  :name "yacc")

(declare-native
  :name "_yacc"
  :source ["yacc.c"])

(declare-source
 :source ["yacc.janet"])