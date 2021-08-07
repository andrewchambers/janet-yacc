
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
  char *act;
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
  char type[IdntSz];
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

Item i0; /* temporary item */

int nrl, nsy, nst, ntk;
Rule rs[MaxRl]; /* grammar rules (ordered, rcmp) */
Info is[MaxTk+MaxNt]; /* symbol information */
Item **st; /* LALR(1) states (ordered, icmp) */
Row *as;   /* action table [state][tok] */
Row *gs;   /* goto table   [sym][state] */
Sym sstart;/* start symbol */
Item *ini; /* initial state */

int srconf, rrconf;
int actsz;
int *act;
int *chk;
int *adsp;
int *gdsp;

FILE *fout;
FILE *fgrm;

static void
die(char *s)
{
  fprintf(stderr, "%s\n", s);
  exit(1);
}

static void *
yalloc(size_t n, size_t o)
{
  void *p;

  p = calloc(n, o);
  if (!p)
    die("out of memory");
  return p;
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
        die("some non-terminals are not defined");
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
            die("too many terms in item");
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

  i0.nt = 0;
  for (n=0, t=i->ts; n<i->nt; n++, t++) {
    if (t->rule->rhs[t->dot] != s)
      continue;
    t1 = &i0.ts[i0.nt++];
    *t1 = *t;
    t1->dot++;
  }
  qsort(i0.ts, i0.nt, sizeof i0.ts[0], tcmpv);
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
    st = realloc(st, ++nst * sizeof st[0]);
    if (!st)
      die("out of memory");
    memmove(&st[hi+1], &st[hi], (nst-1 - hi) * sizeof st[0]);
    i->gtbl = yalloc(nsy, sizeof i->gtbl[0]);
    i->dirty = 1;
    i1 = yalloc(1, sizeof *i1);
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

  ini = &i0;
  r = rfind(Sym0);
  tini.rule = r;
  tini.dot = 0;
  tszero(&tini.lk);
  SetBit(tini.lk.t, 0);
  i0.nt = 0;
  i0.ts[i0.nt++] = tini;
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
        i1 = &i0;
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

static int
resolve(Rule *r, Sym s, int st)
{
  if (!r->prec || !is[s].prec) {
  conflict:
    if (fgrm)
      fprintf(fgrm, srs, st, is[s].name);
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
        if (fgrm)
          fprintf(fgrm, rrs, i->id-1, is[s].name);
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
    fprintf(fgrm, "\n%03d: %s ->", (int)(r-rs), is[r->lhs].name);
    for (s1=r->rhs; *s1!=S; s1++)
      fprintf(fgrm, " %s", is[*s1].name);
  }
  fprintf(fgrm, "\n");
  for (m=0; m<nst; m++) {
    fprintf(fgrm, "\nState %d:\n", m);
    for (t=st[m]->ts; t-st[m]->ts<st[m]->nt; t++) {
      r = t->rule;
      d = t->dot;
      if (d==0 && t!=st[m]->ts)
        continue;
      fprintf(fgrm, "  %s ->", is[r->lhs].name);
      for (s1=r->rhs; *s1!=S; s1++, d--)
        fprintf(fgrm, " %s%s", d ? "" : ". ", is[*s1].name);
      if (!d)
        fprintf(fgrm, " .");
      fprintf(fgrm, "\n");
    }
    fprintf(fgrm, "\n");
    ar = &as[m];
    for (n=0; n<ntk; n++) {
      act = ar->t[n];
      if (!act)
        continue;
      if (act==-1)
        fprintf(fgrm, "  %s error (nonassoc)\n", is[n].name);
      else if (act<0)
        fprintf(fgrm, "  %s reduce with rule %d\n", is[n].name, Red(act));
      else
        fprintf(fgrm, "  %s shift and go to %d\n", is[n].name, act-1);
    }
    if (ar->def != -1)
      fprintf(fgrm, "  * reduce with rule %d\n", ar->def);
  }

  fprintf(fgrm, "%d shift/reduce conflicts\n", srconf);
  fprintf(fgrm, "%d reduce/reduce conflicts\n", rrconf);
}

static void
tblgen()
{
  Row *r;
  Item *i;
  int n, m;

  for (n=0; n<nst; n++)
    st[n]->id = n+1;
  as = yalloc(nst, sizeof as[0]);
  gs = yalloc(nsy-MaxTk, sizeof gs[0]);
  /* fill action table */
  for (n=0; n<nst; n++) {
    r = &as[n];
    r->t = yalloc(ntk, sizeof r->t[0]);
    for (i=st[n], m=0; m<i->nt; m++)
      tblset(r->t, i, &i->ts[m]);
    setdef(r, ntk, -1);
    r->def = Red(r->def); /* Red(-1) == -1 */
  }
  /* fill goto table */
  for (n=MaxTk; n<nsy; n++) {
    r = &gs[n-MaxTk];
    r->t = yalloc(nst, sizeof r->t[0]);
    for (m=0; m<nst; m++)
      if (st[m]->gtbl[n])
        r->t[m] = st[m]->gtbl[n]->id;
    setdef(r, nst, nst+1);
  }

  if (fgrm)
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
  o = yalloc(nst+nsy, sizeof o[0]);
  act = yalloc(nst*nsy, sizeof act[0]);
  chk = yalloc(nst*nsy, sizeof chk[0]);
  adsp = yalloc(nst, sizeof adsp[0]);
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
  gdsp = yalloc(nnt, sizeof gdsp[0]);
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
  free(o);
}

static void
aout(char *name, int *t, int n)
{
  int i;

  fprintf(fout, "short %s[] = {", name);
  for (i=0; i<n; i++) {
    if (i % 10 == 0)
      fprintf(fout, "\n");
    fprintf(fout, "%4d", t[i]);
    if (i != n-1)
      fprintf(fout, ",");
  }
  fprintf(fout, "\n};\n");
}

static void
tblout()
{
  int *o, n, m;

  fprintf(fout, "short yyini = %d;\n", ini->id-1);
  fprintf(fout, "short yyntoks = %d;\n", ntk);
  o = yalloc(nrl+nst+nsy, sizeof o[0]);
  for (n=0; n<nrl; n++)
    o[n] = slen(rs[n].rhs);
  aout("yyr1", o, nrl);
  for (n=0; n<nrl; n++)
    o[n] = rs[n].lhs-MaxTk;
  aout("yyr2", o, nrl);
  for (n=0; n<nst; n++)
    o[n] = as[n].def;
  aout("yyadef", o, nst);
  for (n=0; n<nsy-MaxTk; n++) {
    o[n] = gs[n].def;
    assert(o[n]>0 || o[n]==-1);
    if (o[n]>0)
      o[n]--;
  }
  aout("yygdef", o, nsy-MaxTk);
  aout("yyadsp", adsp, nst);
  aout("yygdsp", gdsp, nsy-MaxTk);
  for (n=0; n<actsz; n++)
    if (act[n]>=0)
      act[n]--;
  aout("yyact", act, actsz);
  aout("yychk", chk, actsz);
  free(o);
}

static Sym
findsy(char *name, int add)
{
  int n;

  for (n=0; n<nsy; n++) {
    if (n == ntk) {
      if (name[0]=='\'') {
        if (ntk>=MaxTk)
          die("too many tokens");
        ntk++;
        strcpy(is[n].name, name);
        return n;
      }
      n = MaxTk;
    }
    if (strcmp(is[n].name, name)==0)
      return n;
  }
  if (add) {
    if (nsy>=MaxTk+MaxNt)
      die("too many non-terminals");
    strcpy(is[nsy].name, name);
    return nsy++;
  } else
    return nsy;
}


int
main(int ac, char *av[])
{
  ginit();
  stgen();
  tblgen();
  actgen();
  tblout();

  exit(0);
}
