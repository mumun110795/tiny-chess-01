import json,os,sys,urllib.request
U=os.environ.get("MAKEFUN_ENV_URL")
V=[0,100,320,330,500,900,0]
KN=[31,33,14,18,-14,-18,-31,-33]
D8=[16,-16,1,-1,17,15,-17,-15]
DG=[17,15,-17,-15];ST=[16,-16,1,-1]
CM={4:12,0:13,7:14,116:3,112:7,119:11}
CF=[0,1,2,3,3,2,1,0];CR=[0,1,2,2,2,2,1,0]
PA=[0,0,3,9,20,38,65,0]
B=[0]*128;W=True;C=0;E=-1;HF=0;SK=[]
def atk(s,by):
 for d in(15,17):
  x=s-d*by
  if not x&136 and B[x]==by:return 1
 for d in KN:
  x=s+d
  if not x&136 and B[x]==2*by:return 1
 for d in D8:
  x=s+d
  if not x&136 and B[x]==6*by:return 1
 for ds,ts in((DG,(3,5)),(ST,(4,5))):
  for d in ds:
   x=s
   while 1:
    x+=d
    if x&136:break
    p=B[x]
    if p:
     if p*by>0 and abs(p)in ts:return 1
     break
 return 0
def enc(f,t,p=0):return f|t<<7|p<<14
def gen():
 sg=1 if W else -1;ms=[]
 for s in range(128):
  if s&136:continue
  pc=B[s]
  if not pc or pc*sg<0:continue
  a=abs(pc);r=s>>4
  if a==1:
   dr=16*sg;o=s+dr
   if not B[o]:
    if o>>4 in(0,7):
     for pr in(5,4,3,2):ms.append(enc(s,o,pr))
    else:
     ms.append(enc(s,o))
     if r==(1 if sg>0 else 6)and not B[s+2*dr]:ms.append(enc(s,s+2*dr))
   for d in(15,17):
    t=s+d*sg
    if t&136:continue
    if B[t]*sg<0 or t==E:
     if t>>4 in(0,7):
      for pr in(5,4,3,2):ms.append(enc(s,t,pr))
     else:ms.append(enc(s,t))
  elif a in(2,6):
   for d in(KN if a==2 else D8):
    t=s+d
    if not t&136 and B[t]*sg<=0:ms.append(enc(s,t))
  elif a in(3,4,5):
   for d in(DG if a==3 else ST if a==4 else D8):
    t=s
    while 1:
     t+=d
     if t&136:break
     if B[t]*sg>0:break
     ms.append(enc(s,t))
     if B[t]:break
  if a==6 and sg>0 and s==4:
   if C&1 and not B[5]and not B[6]and not atk(4,-1)and not atk(5,-1)and not atk(6,-1):ms.append(enc(4,6))
   if C&2 and not B[3]and not B[2]and not B[1]and not atk(4,-1)and not atk(3,-1)and not atk(2,-1):ms.append(enc(4,2))
  if a==6 and sg<0 and s==116:
   if C&4 and not B[117]and not B[118]and not atk(116,1)and not atk(117,1)and not atk(118,1):ms.append(enc(116,118))
   if C&8 and not B[115]and not B[114]and not B[113]and not atk(116,1)and not atk(115,1)and not atk(114,1):ms.append(enc(116,114))
 return ms
def mk(m):
 global W,C,E,HF
 SK.append((B[:],C,E,HF,W))
 f=m&127;t=m>>7&127;p=m>>14;pc=B[f];cap=B[t]
 if abs(pc)==1 and t==E and not cap and f&7!=t&7:B[t-16 if pc>0 else t+16]=0
 B[t]=p*(1 if pc>0 else -1)if p else pc;B[f]=0
 if abs(pc)==6 and abs(t-f)==2:
  if t>f:B[f+1]=B[f+3];B[f+3]=0
  else:B[f-1]=B[f-4];B[f-4]=0
 C&=CM.get(f,15)&CM.get(t,15)
 E=(f+t)//2 if abs(pc)==1 and abs(t-f)==32 else -1
 HF=0 if abs(pc)==1 or cap else HF+1
 W=not W
def um():
 global B,W,C,E,HF
 B,C,E,HF,W=SK.pop()
def legal():
 sg=1 if W else -1;out=[]
 for m in gen():
  mk(m)
  if not atk(B.index(6*sg),-sg):out.append(m)
  um()
 return out
def ev():
 s=0;ph=0;bw=bb=0
 for q in range(128):
  if q&136:continue
  p=B[q]
  if not p:continue
  a=abs(p);sg=1 if p>0 else -1;r=q>>4;f=q&7;orr=r if sg>0 else 7-r
  v=V[a];ph+=(0,0,1,1,2,4,0)[a]
  if a==1:v+=PA[orr]+CF[f]*2
  elif a==2:v+=(CF[f]+CR[r])*6-4
  elif a==3:v+=(CF[f]+CR[r])*3;bw+=sg>0;bb+=sg<0
  elif a==4:v+=CF[f]*2+(10 if orr==6 else 0)
  elif a==5:v+=(CF[f]+CR[r])
  elif a==6:v+=(CF[f]+CR[r])*(5 if ph<10 else -4)
  s+=sg*v
 if bw>1:s+=25
 if bb>1:s-=25
 return s if W else -s
class X(Exception):pass
ND=[0];BUD=24000;TT={};KIL={};HIS={};PTH=set()
def key():return(bytes([x+6 for x in B]),W,C,E)
def ordr(ms,tm):
 def sc(m):
  f=m&127;t=m>>7&127;v=B[t]
  if abs(B[f])==1 and t==E and not v:v=-1
  s=HIS.get(m,0)
  if m==tm:s=1<<24
  elif v:s=1<<20+V[abs(v)]*10-V[abs(B[f])]//10
  elif KIL.get(PY)==m:s=1<<18
  return(-s,m)
 return sorted(ms,key=sc)
PY=0
def qs(a,b,ply):
 global PY
 ND[0]+=1
 if ND[0]%512==0 and ND[0]>BUD:raise X
 sg=1 if W else -1;ic=atk(B.index(6*sg),-sg)
 st=-100000 if ic else ev()
 if not ic:
  if st>=b:return st
  a=max(a,st)
 ms=legal()
 if not ms:return -100000+ply if ic else st
 if not ic:ms=[m for m in ms if B[m>>7&127]or m>>14 or(abs(B[m&127])==1 and(m>>7&127)==E)]
 ms=ordr(ms,-1);PY=ply
 for m in ms:
  mk(m);v=-qs(-b,-a,ply+1);um()
  if v>a:
   a=v
   if a>=b:break
 return a
def sr(d,a,b,ply):
 global PY
 if d<=0:return qs(a,b,ply)
 ND[0]+=1
 if ND[0]%512==0 and ND[0]>BUD:raise X
 if HF>=100:return 0
 k=key()
 if k in PTH:return 0
 e=TT.get(k)
 tm=-1
 if e and e[0]>=d:
  if e[2]==0:return e[1]
  if e[2]==1:a=max(a,e[1])
  else:b=min(b,e[1])
  if a>=b:return e[1]
 if e:tm=e[3]
 ms=legal()
 if not ms:
  sg=1 if W else -1
  return -100000+ply if atk(B.index(6*sg),-sg)else 0
 ms=ordr(ms,tm);PY=ply;PTH.add(k);best=-1;fl=2;v0=a
 for m in ms:
  mk(m);v=-sr(d-1,-b,-a,ply+1);um()
  if v>a:
   a=v;best=m;fl=0
   if a>=b:fl=1;KIL[ply]=m;break
  HIS[m]=HIS.get(m,0)+d*d
 PTH.discard(k);TT[k]=(d,a,fl,best)
 return a
def uci(m):
 f=m&127;t=m>>7&127
 u="abcdefgh"[f&7]+str((f>>4)+1)+"abcdefgh"[t&7]+str((t>>4)+1)
 return u+({5:"q",4:"r",3:"b",2:"n"}[m>>14]if m>>14 else"")
def parse(fn):
 global B,W,C,E,HF
 B=[0]*128;ps=fn.split();rows=ps[0].split("/")
 pm={"p":1,"n":2,"b":3,"r":4,"q":5,"k":6}
 for i,row in enumerate(rows):
  f=0
  for ch in row:
   if ch.isdigit():f+=int(ch)
   else:B[(7-i)*16+f]=pm[ch.lower()]*(1 if ch.isupper()else-1);f+=1
 W=ps[1]=="w";C=0
 if len(ps)>2:
  for ch in ps[2]:
   C|={"K":1,"Q":2,"k":4,"q":8}.get(ch,0)
 E=-1
 if len(ps)>3 and ps[3]!="-":E="abcdefgh".index(ps[3][0])+(int(ps[3][1])-1)*16
 HF=int(ps[4])if len(ps)>4 else 0
def best(fn):
 global TT,KIL,HIS
 parse(fn);ms=legal()
 if len(ms)==1:return uci(ms[0])
 TT={};KIL={};HIS={};ND[0]=0;bm=ms[0]
 for d in range(1,30):
  try:
   a=-100000;cm=bm
   for m in ordr(ms,bm):
    mk(m);v=-sr(d-1,-100000,-a,1)if a>-100000 else -sr(d-1,-100000,100000,1);um()
    if v>a:a=v;cm=m
   bm=cm
  except X:break
  if ND[0]>BUD:break
 return uci(bm)
def post(p,bd):
 q=urllib.request.Request(U+p,data=json.dumps(bd).encode(),headers={"content-type":"application/json"})
 with urllib.request.urlopen(q)as r:return json.load(r)
if __name__=="__main__" and U:
 while 1:
  g=post("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   mv=best(g["fen"])
   if mv not in g["legal_moves"]:mv=g["legal_moves"][0]
   g=post("/move",{"game":g["game"],"move":mv})
