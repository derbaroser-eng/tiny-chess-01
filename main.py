import sys
S=(18,33,14,31,-18,-33,-14,-31);B=(17,16,15,1,-1,-15,-16,-17);R=(16,1,-1,-16)
V={'p':100,'n':320,'b':330,'r':500,'q':900,'k':20000}
def ok(s):return not s&136
def parse(f):
 z=f.split();b=[' ']*128;i=0;r=7
 for c in z[0]:
  if c=='/':r-=1;i=0
  elif c.isdigit():i+=int(c)
  else:b[r*16+i]=c;i+=1
 e=z[3];ep=-1 if e=='-' else ord(e[0])-97+16*(ord(e[1])-49)
 return b,z[1],z[2],ep
def att(b,s,w):
 if any(ok(s+x) and b[s+x]==('N' if w else 'n') for x in S):return 1
 for x in B:
  t=s+x
  while ok(t):
   p=b[t]
   if p!=' ':
    if p.isupper()==w and p.lower() in 'bq':return 1
    break
   t+=x
 for x in R:
  t=s+x
  while ok(t):
   p=b[t]
   if p!=' ':
    if p.isupper()==w and p.lower() in 'rq':return 1
    break
   t+=x
 for x in B+R:
  t=s+x
  if ok(t) and b[t]==('K' if w else 'k'):return 1
 d=16 if w else -16
 return any(ok(s-d+x) and b[s-d+x]==('P' if w else 'p') for x in(-1,1))
def gen(b,t,cast,ep):
 m=[];ew=t!='w'
 for s in range(128):
  if not ok(s):continue
  p=b[s]
  if p==' ' or p.isupper()!=(t=='w'):continue
  r=s>>4;mine=p.isupper()
  if p.lower()=='p':
   d=16 if t=='w' else -16;q=s+d
   if ok(q) and b[q]==' ':
    if (q>>4) in(0,7):
     for c in'qrbn':m.append((s,q,c))
    else:
     m.append((s,q,''))
     if r in(1,6) and b[s+2*d]==' ':m.append((s,s+2*d,''))
   for x in(-1,1):
    q=s+d+x
    if ok(q) and((b[q]!=' ' and b[q].isupper()!=mine)or q==ep):
     if (q>>4) in(0,7):
      for c in'qrbn':m.append((s,q,c))
     else:m.append((s,q,''))
  elif p.lower()=='n':
   for x in S:
    q=s+x
    if ok(q) and(b[q]==' ' or b[q].isupper()!=mine):m.append((s,q,''))
  elif p.lower()=='k':
   for x in B+R:
    q=s+x
    if ok(q) and(b[q]==' ' or b[q].isupper()!=mine):m.append((s,q,''))
   if not att(b,s,ew):
    W=t=='w'
    if W and s==4:
     if 'K' in cast and b[5]==' ' and b[6]==' ' and not att(b,5,ew) and not att(b,6,ew):m.append((4,6,''))
     if 'Q' in cast and b[3]==' ' and b[2]==' ' and b[1]==' ' and not att(b,3,ew) and not att(b,2,ew):m.append((4,2,''))
    if not W and s==116:
     if 'k' in cast and b[117]==' ' and b[118]==' ' and not att(b,117,ew) and not att(b,118,ew):m.append((116,118,''))
     if 'q' in cast and b[115]==' ' and b[114]==' ' and b[113]==' ' and not att(b,115,ew) and not att(b,114,ew):m.append((116,114,''))
  else:
   for x in {'b':B,'r':R,'q':B+R}[p.lower()]:
    q=s+x
    while ok(q):
     if b[q]==' ':m.append((s,q,''));q+=x
     else:
      if b[q].isupper()!=mine:m.append((s,q,''))
      break
 return m
def move(b,m,t,ep):
 s,q,c=m;p=b[s];cap=-1
 if p.lower()=='p' and q==ep and b[q]==' ':cap=q-16 if t=='w' else q+16
 b[q]=(c.upper() if t=='w' else c) if c else p;b[s]=' '
 if cap>=0:b[cap]=' '
 if p.lower()=='k' and abs(q-s)==2:
  if q>s:b[q-1]=b[q+1];b[q+1]=' '
  else:b[q+1]=b[q-2];b[q-2]=' '
 return (s+q)//2 if p.lower()=='p' and abs(q-s)==32 else -1
def legal(b,t,cast,ep):
 o=[];k='K' if t=='w' else 'k';ew=t!='w'
 for m in gen(b,t,cast,ep):
  c=b[:];move(c,m,t,ep)
  if not att(c,c.index(k),ew):o.append(m)
 return o
def ev(b,t):
 x=0
 for p in b:
  if p.isalpha():x+=V[p.lower()]*(1 if p.isupper() else -1)
 return x if t=='w'else -x
def srch(b,t,d,a,be,n):
 n[0]+=1
 ms=legal(b,t,'',-1);k='K' if t=='w' else 'k';ew=t!='w'
 if not ms:return -99999 if att(b,b.index(k),ew) else 0
 if d<=0 or n[0]>60000:return ev(b,t)
 ms.sort(key=lambda m:-((V.get(b[m[1]].lower(),0) if b[m[1]]!=' ' else 0)+(V.get(m[2],0) if m[2] else 0)))
 z=-99999
 for m in ms:
  c=b[:];move(c,m,t,-1);sc=-srch(c,t!='w',d-1,-be,-a,n)
  if sc>z:z=sc
  if z>a:a=z
  if a>=be:break
 return z
def sq(i):return chr(97+(i&15))+str((i>>4)+1)
def best(f,d=2):
 b,t,cast,ep=parse(f);ms=legal(b,t,cast,ep)
 if not ms:return'0000'
 bm=None;bs=-99999
 for m in ms:
  c=b[:];move(c,m,t,ep);sc=-srch(c,t!='w',d-1,-99999,99999,[0])
  if sc>bs:bs=sc;bm=m
 s,q,pr=bm
 return sq(s)+sq(q)+(pr or '')
if __name__=='__main__':
 f=''
 for l in sys.stdin:
  l=l.strip()
  if l.startswith('position fen '):f=l[13:]
  elif l.startswith('fen '):print(best(l[4:]));sys.stdout.flush()
  elif l=='go':print('bestmove '+best(f));sys.stdout.flush()
