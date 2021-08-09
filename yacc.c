// An implementation of yacc based on
// https://c9x.me/git/miniyacc.git/tree/yacc.c
//
// To understand the implementation you must read https://c9x.me/yacc/.
//
// To port to janet we have made a few changes:
//
// - The parser is now walking janet data structures instead of parsing text.
// - All allocation uses the janet scratch allocator.
// - We abort using janet panics.
// - Added a new output table yyfns to map action numbers to functions.
// - Changed the purpose of yytrn to map janet keywords to token numbes.

#include <janet.h>

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int Sym;
typedef struct Rule Rule;
typedef struct TSet TSet;
typedef struct Info Info;
typedef struct Term Term;
typedef struct Item Item;
typedef struct Row Row;

#define S ((Sym) -1)
#define Red(n) (- (n+2)) /* involutive, Red(Red(x)) == x */
#define GetBit(s,n) (s[n/32] & (1<<(n%32)))
#define SetBit(s,n) (s[n/32] |= 1<<(n%32))

enum {
  IdntSz = 64,
  MaxRhs = 32,
  MaxTk = 500,
  MaxNt = 500,
  MaxRl = 800,
  MaxTm = 1000,

  TSetSz = (MaxTk+31)/32,
  Sym0 = MaxTk
};

struct Rule {
  Sym lhs;
  Sym rhs[MaxRhs];
  Janet act;
  int actln;
  int prec;
};

struct TSet {
  unsigned t[TSetSz];
};

struct Info {
  int nul;
  TSet fst;
  int prec;
  enum {
    ANone,
    ALeft,
    ARight,
    ANonassoc
  } assoc;
  char name[IdntSz];
};

struct Term {
  Rule *rule;
  int dot;
  TSet lk;
};

struct Item {
  int id;
  int nt;
  Term ts[MaxTm];
  Item **gtbl;
  int dirty;
};

struct Row {
  int def;
  int ndef;
  int *t;
};

static char srs[] = "shift/reduce conflict state %d token %s\n";
static char rrs[] = "reduce/reduce conflict state %d token %s\n";

#define TL JANET_THREAD_LOCAL

TL Item *i0; /* temporary item */
TL Rule *rs; /* grammar rules (ordered, rcmp) */
TL Info *is; /* symbol information */
TL int nrl, nsy, nst, ntk;
TL Item **st; /* LALR(1) states (ordered, icmp) */
TL Row *as;   /* action table [state][tok] */
TL Row *gs;   /* goto table   [sym][state] */
TL Sym sstart;/* start symbol */
TL Item *ini; /* initial state */

TL int dbgon;
TL int srconf, rrconf;
TL int actsz;
TL int *act;
TL int *chk;
TL int *adsp;
TL int *gdsp;

#undef TL

static void
reset()
{
  i0 = janet_scalloc(1, sizeof(Item));
  rs = janet_scalloc(MaxRl, sizeof(Rule));
  is = janet_scalloc(MaxTk+MaxNt, sizeof(Info));
  nrl = 0;
  nsy = 0;
  nst = 0;
  ntk = 0;
  st = 0; 
  as = 0;
  gs = 0;
  sstart = 0;
  ini = 0;
  dbgon = 0;
  srconf = 0;
  rrconf = 0;
  actsz = 0;
  act = 0;
  chk = 0;
  adsp = 0;
  gdsp = 0;
}


static int
rcmp(const void *a, const void *b)
{
  return ((Rule *)a)->lhs - ((Rule *)b)->lhs;
}

static Rule *
rfind(Sym lhs)
{
  Rule *r;
  Rule k;

  k.lhs = lhs;
  r = bsearch(&k, rs, nrl, sizeof *r, rcmp);
  if (r != 0)
    while (r > rs && r[-1].lhs == lhs)
      r--;
  return r;
}

static int
slen(Sym *l)
{
  int n;

  for (n=0; *l!=S; n++, l++);
  return n;
}

static void
tszero(TSet *ts)
{
  memset(ts, 0, sizeof *ts);
}

static int
tsunion(TSet *tsa, TSet *tsb)
{
  int n;
  unsigned *a, *b, c, t;

  c = 0;
  a = tsa->t;
  b = tsb->t;
  n = (31+ntk)/32;
  while (n-- > 0) {
    t = *a;
    *a |= *b++;
    c |= t ^ *a++;
  }
  return !!c;
}

static void
first(TSet *ts, Sym *stnc, TSet *last)
{
  Sym f;

  f = stnc[0];
  if (f == S) {
    if (last)
      tsunion(ts, last);
    return;
  }
  if (f < ntk) {
    SetBit(ts->t, f);
    return;
  }
  if (is[f].nul)
    first(ts, stnc+1, last);
  tsunion(ts, &is[f].fst);
}

static void
ginit()
{
  int chg;
  Rule *r;
  Info *i;
  Sym *s;
  TSet ts;

  do {
    chg = 0;
    for (r=rs; r-rs<nrl; r++) {
      i = &is[r->lhs];
      for (s=r->rhs; *s!=S; s++)
        if (!is[*s].nul)
          goto nonul;
      chg |= i->nul == 0;
      i->nul = 1;
    nonul:
      tszero(&ts);
      first(&ts, r->rhs, 0);
      chg |= tsunion(&i->fst, &ts);
    }
  } while (chg);
}

static int
tcmp(Term *a, Term *b)
{
  int c;

  c = a->rule - b->rule;
  if (c==0)
    c = a->dot - b->dot;
  return c;
}

static int
tcmpv(const void *a, const void *b)
{
  return tcmp((Term *)a, (Term *)b);
}

static void
iclose(Item *i)
{
  int smap[MaxNt];
  Rule *r;
  Term *t, t1;
  Sym s, *rem;
  int chg, n, m;

  t1.dot = 0;
  memset(smap, 0, sizeof smap);
  for (n=0; n<i->nt; n++) {
    t = &i->ts[n];
    s = t->rule->lhs-Sym0;
    if (t->dot==0)
    if (smap[s]==0)
      smap[s] = n;
  }
  do {
    chg = 0;
    for (n=0; n<i->nt; n++) {
      t = &i->ts[n];
      rem = &t->rule->rhs[t->dot];
      s = *rem++;
      if (s < Sym0 || s == S)
        continue;
      r = rfind(s);
      if (!r)
        janet_panic("some non-terminals are not defined");
      tszero(&t1.lk);
      first(&t1.lk, rem, &t->lk);
      m = smap[s-Sym0];
      if (m)
        for (; r-rs<nrl && r->lhs==s; r++, m++)
          chg |= tsunion(&i->ts[m].lk, &t1.lk);
      else {
        m = i->nt;
        smap[s-Sym0] = m;
        for (; r-rs<nrl && r->lhs==s; r++, m++) {
          if (m>=MaxTm)
            janet_panic("too many terms in item");
          t1.rule = r;
          i->ts[m] = t1;
        }
        i->nt = m;
        chg = 1;
      }
    }
  } while (chg);
}

static void
igoto(Item *i, Sym s)
{
  Term *t, *t1;
  int n;

  i0->nt = 0;
  for (n=0, t=i->ts; n<i->nt; n++, t++) {
    if (t->rule->rhs[t->dot] != s)
      continue;
    t1 = &i0->ts[i0->nt++];
    *t1 = *t;
    t1->dot++;
  }
  qsort(i0->ts, i0->nt, sizeof i0->ts[0], tcmpv);
}

static int
icmp(Item *a, Item *b)
{
  Term *ta, *tb, *ma, *mb;
  int c;

  ta = a->ts;
  tb = b->ts;
  ma = ta+a->nt;
  mb = tb+b->nt;
  for (;;) {
    if (ta==ma || ta->dot==0)
      return -(tb<mb && tb->dot);
    if (tb==mb || tb->dot==0)
      return +(ta<ma && ta->dot);
    if ((c=tcmp(ta++, tb++)))
      return c;
  }
}

static int
stadd(Item **pi)
{
  Item *i, *i1;
  int lo, hi, mid, n, chg;

  /* http://www.iq0.com/duffgram/bsearch.c */
  i = *pi;
  lo = 0;
  hi = nst - 1;
  if (hi<0 || icmp(i, st[hi])>0)
    hi++;
  else if (icmp(i, st[lo])<=0)
    hi = lo;
  else
    while (hi-lo!=1) {
      mid = (lo+hi)/2;
      if (icmp(st[mid], i)<0)
        lo = mid;
      else
        hi = mid;
    }
  if (hi<nst && icmp(st[hi], i)==0) {
    chg = 0;
    i1 = st[hi];
    for (n=0; n<i->nt; n++)
      chg |= tsunion(&i1->ts[n].lk, &i->ts[n].lk);
    i1->dirty |= chg;
    *pi = i1;
    return chg;
  } else {
    st = janet_srealloc(st, ++nst * sizeof st[0]);
    if (!st)
      janet_panic("out of memory");
    memmove(&st[hi+1], &st[hi], (nst-1 - hi) * sizeof st[0]);
    i->gtbl = janet_scalloc(nsy, sizeof i->gtbl[0]);
    i->dirty = 1;
    i1 = janet_scalloc(1, sizeof *i1);
    *i1 = *i;
    *pi = st[hi] = i1;
    return 1;
  }
}

static void
stgen()
{
  Sym s;
  Rule *r;
  Item *i, *i1;
  Term tini;
  int n, chg;

  ini = i0;
  r = rfind(Sym0);
  tini.rule = r;
  tini.dot = 0;
  tszero(&tini.lk);
  SetBit(tini.lk.t, 0);
  i0->nt = 0;
  i0->ts[i0->nt++] = tini;
  stadd(&ini);
  do {
    chg = 0;
    for (n=0; n<nst; n++) {
      i = st[n];
      if (!i->dirty)
        continue;
      i->dirty = 0;
      iclose(i);
      for (s=0; s<nsy; s++) {
        igoto(i, s);
        i1 = i0;
        if (!i1->nt) {
          i->gtbl[s] = 0;
          continue;
        }
        chg |= stadd(&i1);
        i->gtbl[s] = i1;
      }
    }
  } while (chg);
}


#define dbgprintf(...) janet_dynprintf("yydbg", stderr, __VA_ARGS__)

static int
resolve(Rule *r, Sym s, int st)
{
  if (!r->prec || !is[s].prec) {
  conflict:
    if (dbgon)
      dbgprintf(srs, st, is[s].name);
    srconf++;
    return ARight;
  }
  if (r->prec==is[s].prec) {
    if (is[s].assoc == ANone)
      goto conflict;
    return is[s].assoc;
  } else
    if (r->prec<is[s].prec)
      return ARight;
    else
      return ALeft;
}

static void
tblset(int *tbl, Item *i, Term *t)
{
  int act;
  Sym s;

  s = t->rule->rhs[t->dot];
  if (s!=S) {
    /* shift */
    if (s>=ntk)
      return;
    assert(i->gtbl[s]);
    act = ARight;
    if (tbl[s] && tbl[s] != i->gtbl[s]->id) {
      assert(tbl[s]<=0);
      act = resolve(&rs[Red(tbl[s])], s, i->id-1);
    }
    switch (act) {
    case ARight:
      tbl[s] = i->gtbl[s]->id;
      break;
    case ANonassoc:
      tbl[s] = -1;
      break;
    }
  } else
    /* reduce */
    for (s=0; s<ntk; s++) {
      if (!GetBit(t->lk.t, s))
        continue;
      /* default to shift if conflict occurs */
      if (!tbl[s])
        act = ALeft;
      else if (tbl[s]<0) {
        if (dbgon)
          dbgprintf(rrs, i->id-1, is[s].name);
        rrconf++;
        act = ARight;
      } else
        act = resolve(t->rule, s, i->id-1);
      switch (act) {
      case ALeft:
        tbl[s] = Red(t->rule-rs);
        break;
      case ANonassoc:
        tbl[s] = -1;
        break;
      }
    }
}

static void
setdef(Row *r, int w, int top)
{
  int n, m, x, cnt, def, max;

  max = 0;
  def = -1;
  r->ndef = 0;
  for (n=0; n<w; n++) {
    x = r->t[n];
    if (x==0)
      r->ndef++;
    if (x>=top || x==0)
      continue;
    cnt = 1;
    for (m=n+1; m<w; m++)
      if (r->t[m]==x)
        cnt++;
    if (cnt>max) {
      def = x;
      max = cnt;
    }
  }
  r->def = def;
  if (max!=0)
    /* zero out the most frequent entry */
    for (n=0; n<w; n++)
      if (r->t[n]==def) {
        r->t[n] = 0;
        r->ndef++;
      }
}

static void
stdump()
{
  Term *t;
  Sym *s1;
  int n, m, d, act;
  Rule *r;
  Row *ar;

  for (r=rs; r-rs<nrl; r++) {
    dbgprintf("\n%03d: %s ->", (int)(r-rs), is[r->lhs].name);
    for (s1=r->rhs; *s1!=S; s1++)
      dbgprintf(" %s", is[*s1].name);
  }
  dbgprintf("\n");
  for (m=0; m<nst; m++) {
    dbgprintf("\nState %d:\n", m);
    for (t=st[m]->ts; t-st[m]->ts<st[m]->nt; t++) {
      r = t->rule;
      d = t->dot;
      if (d==0 && t!=st[m]->ts)
        continue;
      dbgprintf("  %s ->", is[r->lhs].name);
      for (s1=r->rhs; *s1!=S; s1++, d--)
        dbgprintf(" %s%s", d ? "" : ". ", is[*s1].name);
      if (!d)
        dbgprintf(" .");
      dbgprintf("\n");
    }
    dbgprintf("\n");
    ar = &as[m];
    for (n=0; n<ntk; n++) {
      act = ar->t[n];
      if (!act)
        continue;
      if (act==-1)
        dbgprintf("  %s error (nonassoc)\n", is[n].name);
      else if (act<0)
        dbgprintf("  %s reduce with rule %d\n", is[n].name, Red(act));
      else
        dbgprintf("  %s shift and go to %d\n", is[n].name, act-1);
    }
    if (ar->def != -1)
      dbgprintf("  * reduce with rule %d\n", ar->def);
  }
  dbgprintf("\n");
  dbgprintf("%d shift/reduce conflicts\n", srconf);
  dbgprintf("%d reduce/reduce conflicts\n", rrconf);
}

static void
tblgen()
{
  Row *r;
  Item *i;
  int n, m;

  for (n=0; n<nst; n++)
    st[n]->id = n+1;
  as = janet_scalloc(nst, sizeof as[0]);
  gs = janet_scalloc(nsy-MaxTk, sizeof gs[0]);
  /* fill action table */
  for (n=0; n<nst; n++) {
    r = &as[n];
    r->t = janet_scalloc(ntk, sizeof r->t[0]);
    for (i=st[n], m=0; m<i->nt; m++)
      tblset(r->t, i, &i->ts[m]);
    setdef(r, ntk, -1);
    r->def = Red(r->def); /* Red(-1) == -1 */
  }
  /* fill goto table */
  for (n=MaxTk; n<nsy; n++) {
    r = &gs[n-MaxTk];
    r->t = janet_scalloc(nst, sizeof r->t[0]);
    for (m=0; m<nst; m++)
      if (st[m]->gtbl[n])
        r->t[m] = st[m]->gtbl[n]->id;
    setdef(r, nst, nst+1);
  }

  if (dbgon)
    stdump();
}

static int
prcmp(const void *a, const void *b)
{
  return (*(Row **)a)->ndef - (*(Row **)b)->ndef;
}

static void
actgen()
{
  Row **o, *r;
  int n, m, t, dsp, nnt;

  actsz = 0;
  o = janet_scalloc(nst+nsy, sizeof o[0]);
  act = janet_scalloc(nst*nsy, sizeof act[0]);
  chk = janet_scalloc(nst*nsy, sizeof chk[0]);
  adsp = janet_scalloc(nst, sizeof adsp[0]);
  for (n=0; n<nst*nsy; n++)
    chk[n] = -1;
  /* fill in actions */
  for (n=0; n<nst; n++)
    o[n] = &as[n];
  qsort(o, nst, sizeof o[0], prcmp);
  for (n=0; n<nst; n++) {
    r = o[n];
    dsp = 0;
    for (m=0; m<ntk && r->t[m]==0; m++)
      dsp--;
  retrya:
    /* The invariant here is even
     * trickier than it looks.
     */
    for (t=0; t<ntk; t++)
      if ((m=dsp+t)>=0 && chk[m]>=0)
      if ((r->t[t] && (chk[m]!=t || act[m]!=r->t[t]))
      || (!r->t[t] && chk[m]==t)) {
        dsp++;
        goto retrya;
      }
    adsp[r-as] = dsp;
    for (t=0; t<ntk; t++)
      if (r->t[t]) {
        chk[dsp+t] = t;
        act[dsp+t] = r->t[t];
        if (dsp+t>=actsz)
          actsz = dsp+t+1;
      }
  }
  /* fill in gotos */
  nnt = nsy-MaxTk;
  gdsp = janet_scalloc(nnt, sizeof gdsp[0]);
  for (n=0; n<nnt; n++)
    o[n] = &gs[n];
  qsort(o, nnt, sizeof o[0], prcmp);
  for (n=0; n<nnt; n++) {
    r = o[n];
    dsp = 0;
    for (m=0; m<nst && r->t[m]==0; m++)
      dsp--;
  retryg:
    for (t=m; t<nst; t++)
      if (chk[dsp+t]>=0 && r->t[t]) {
        dsp++;
        goto retryg;
      }
    gdsp[r-gs] = dsp;
    for (t=m; t<nst; t++)
      if (r->t[t]) {
        chk[dsp+t] = ntk+(r-gs);
        act[dsp+t] = r->t[t];
        if (dsp+t>=actsz)
          actsz = dsp+t+1;
      }
  }
}

static void
aout(JanetKV *sout, char *name, int *t, int n)
{
  Janet k, v, *tout;
  int i;

  tout = janet_tuple_begin(n);
  for (i=0; i<n; i++) {
    tout[i] = janet_wrap_integer(t[i]);
  }
  k = janet_ckeywordv(name);
  v = janet_wrap_tuple(janet_tuple_end(tout));
  janet_struct_put(sout, k, v);
}

static Janet
tblout()
{
  int *o, n;
  Janet *fns;
  JanetKV *sout, *trns;

  sout = janet_struct_begin(12);

  janet_struct_put(
    sout, 
    janet_ckeywordv("yyini"),
    janet_wrap_integer(ini->id-1)
  );
  janet_struct_put(
    sout,
    janet_ckeywordv("yyntoks"),
    janet_wrap_integer(ntk)
  );

  o = janet_scalloc(nrl+nst+nsy, sizeof o[0]);
  for (n=0; n<nrl; n++)
    o[n] = slen(rs[n].rhs);
  aout(sout, "yyr1", o, nrl);
  for (n=0; n<nrl; n++)
    o[n] = rs[n].lhs-MaxTk;
  aout(sout, "yyr2", o, nrl);
  for (n=0; n<nst; n++)
    o[n] = as[n].def;
  aout(sout, "yyadef", o, nst);
  for (n=0; n<nsy-MaxTk; n++) {
    o[n] = gs[n].def;
    assert(o[n]>0 || o[n]==-1);
    if (o[n]>0)
      o[n]--;
  }
  aout(sout, "yygdef", o, nsy-MaxTk);
  aout(sout, "yyadsp", adsp, nst);
  aout(sout, "yygdsp", gdsp, nsy-MaxTk);
  for (n=0; n<actsz; n++)
    if (act[n]>=0)
      act[n]--;
  aout(sout, "yyact", act, actsz);
  aout(sout, "yychk", chk, actsz);

  trns = janet_struct_begin(ntk-1);
  for (n=1; n<ntk; n++)
    janet_struct_put(
      trns,
      janet_ckeywordv(is[n].name+1),
      janet_wrap_integer(n)
    );
  janet_struct_put(
    sout, 
    janet_ckeywordv("yytrns"),
    janet_wrap_struct(janet_struct_end(trns))
  );

  fns = janet_tuple_begin(nrl);
  for (n=0; n<nrl; n++) {
    fns[n] = rs[n].act;
  }
  janet_struct_put(
    sout, 
    janet_ckeywordv("yyfns"),
    janet_wrap_tuple(janet_tuple_end(fns))
  );

  return janet_wrap_struct(janet_struct_end(sout));
}

static Sym
findsy(Janet v, int add)
{
  char *name, namelen;
  int istok, n;

  if (janet_checktype(v, JANET_SYMBOL)) {
    istok = 0;
    name = (char*)janet_unwrap_symbol(v);
  } else if (janet_checktype(v, JANET_KEYWORD)) {
    istok = 1;
    name = (char*)janet_unwrap_keyword(v);
  } else {
    janet_panicf("tokens and non-terminals must be keywords and symbols respectively, got %p", v);
  }

  namelen = janet_string_length(name);
  // -1 gives us space for null terminator and :.
  if (namelen-2 >= IdntSz)
    janet_panicf("token/non terminal name '%v' too long", v);

  if (istok) {
    for (n=0; n<ntk; n++)
      if (strcmp(is[n].name+1, name)==0)
        return n;
    if (ntk>=MaxTk)
      janet_panic("too many tokens");
    ntk++;
    is[n].name[0] = ':';
    strcpy(is[n].name+1, name);
    return n;
  }

  for (n=MaxTk; n<nsy; n++)
    if (strcmp(is[n].name, name)==0)
      return n;

  if (add) {
    if (nsy>=MaxTk+MaxNt)
      janet_panic("too many non-terminals");
    strcpy(is[nsy].name, name);
    return nsy++;
  } else
    return nsy;
}

static void
addtok(Janet v, int p, int a) {
  Info *si;
  int n;
  if (!janet_checktype(v, JANET_KEYWORD))
    janet_panicf("tokens must be keywords, got %v", v);
  n = findsy(v, 0);
  si = &is[n];
  si->prec = p;
  si->assoc = a;
}

static void
parserules(int32_t nrules, const Janet *rules) {
  const Janet *rule, *pat;
  int32_t i, j, k, nargs, npat;

  Sym hd, *p, s;
  Rule *r;

  if (nrules <= 0)
    janet_panic("at least one rule is required");

  for (i = 0; i < nrules; i++) {

    if (!janet_checktype(rules[i], JANET_TUPLE))
      janet_panicf("grammar rule must be a tuple, got %p", rules[i]);

    rule = janet_unwrap_tuple(rules[i]);
    nargs = janet_tuple_length(rule);

    if (nargs % 2 != 1)
      janet_panicf("rule needs an odd number of elements");

    if (!janet_checktype(rule[0], JANET_SYMBOL))
      janet_panicf("non terminals must be symbols, got %p", rule[0]);

    hd = findsy(rule[0], 1);
    if (sstart==S)
      sstart = hd;

    for (j = 1; j < nargs; j += 2) {

      if (nrl>=MaxRl-1)
        janet_panic("too many rules");
      r = &rs[nrl++];
      r->lhs = hd;
      r->act = janet_wrap_nil();
      p = r->rhs;

      if (!janet_checktype(rule[j], JANET_TUPLE))
        janet_panicf("rule pattern must be a tuple, got %p", rule[j]);

      pat = janet_unwrap_tuple(rule[j]);
      npat = janet_tuple_length(pat);
      if (!janet_symeq(rule[j+1], "_"))
        r->act = rule[j+1];

      for (k = 0; k < npat; k++) {
        if (janet_symeq(pat[k], "%prec")) {
          if (k+1 == npat
              || (s=findsy(pat[k+1], 0))>=ntk)
            janet_panic("token expected after %prec");
          r->prec = is[s].prec;
          k++;
          continue;
        }

        s = findsy(pat[k], 1);
        *p++ = s;
        if (s<ntk && is[s].prec>0)
          r->prec = is[s].prec;
        if (p-r->rhs >= MaxRhs-1)
          janet_panic("production rule too long");
      }

      *p = S;
    }
  }

  r = &rs[nrl++];
  r->lhs = Sym0;
  r->rhs[0] = sstart;
  r->rhs[1] = 0;
  r->rhs[2] = S;
  r->act = janet_ckeywordv("done");
  qsort(rs, nrl, sizeof rs[0], rcmp);
}

static void
parse(int32_t nstmts, const Janet *stmts)
{
  const Janet *stmt;
  int32_t prec, p, i, j, nelems;
  
  strcpy(is[0].name, "$");
  ntk = 1;
  strcpy(is[Sym0].name, "@start");
  nsy = MaxTk+1;
  sstart = S;
  prec = 0;

  for (i=0; i < nstmts; i++) {
    
    if (!janet_checktype(stmts[i], JANET_TUPLE) || janet_tuple_length(janet_unwrap_tuple(stmts[i])) <= 0)
      janet_panicf("expected a statment tuple, got %p", stmts[i]);

    stmt  = janet_unwrap_tuple(stmts[i]);
    nelems = janet_tuple_length(stmt);

    if (janet_symeq(stmt[0], "%start")) {
      if (nelems != 2 || !janet_checktype(stmt[1], JANET_SYMBOL))
        janet_panicf("start expects a single symbol");

      sstart = findsy(stmt[1], 1);
      if (sstart<ntk)
        janet_panic("start cannot specify a token");
    } else if (janet_symeq(stmt[0], "%token")) {
      for (j = 1; j < nelems; j++) {
        addtok(stmt[j], 0, ANone);
      }
    } else if (janet_symeq(stmt[0], "%left")) {
      p = ++prec;
      for (j = 1; j < nelems; j++) {
        addtok(stmt[j], p, ALeft);
      }
    } else if (janet_symeq(stmt[0], "%right")) {
      p = ++prec;
      for (j = 1; j < nelems; j++) {
        addtok(stmt[j], p, ARight);
      }
    } else if (janet_symeq(stmt[0], "%nonassoc")) {
      p = ++prec;
      for (j = 1; j < nelems; j++) {
        addtok(stmt[j], p, ANonassoc);
      }
    } else {
      parserules(nstmts-i, stmts+i);
      return;
    }
  }

  janet_panic("expected rules after delcarations");
}

static Janet
compile(int32_t argc, Janet *argv) {
  janet_arity(argc, 0, 100000);
  reset();
  dbgon = !janet_checktype(janet_dyn("yydbg"), JANET_NIL);
  parse(argc, argv);
  ginit();
  stgen();
  tblgen();
  actgen();
  return tblout();
}

static const JanetReg cfuns[] = {
  {"compile", compile, NULL},
  {NULL, NULL, NULL}
};

JANET_MODULE_ENTRY(JanetTable *env) {
  janet_cfuns(env, "_yacc", cfuns);
}