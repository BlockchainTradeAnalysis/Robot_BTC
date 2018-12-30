library(quantmod)
stk = get(getSymbols("2330.tw"))
chartSeries(stk)
duration = "2018-01-01::2018-12-31"
chartSeries(stk[duration]
            ,theme = "white"
            ,up.col = "red",dn.col="green")
addSMA(5,col="red")
addSMA(10,col="blue")
addSMA(20,col="green")
addSMA(60,col="black")
head(cbind(Cl(stk),SMA(na.omit(Cl(stk)),5)),11)
addBBands()
addRSI()
addMACD()
duration = "2018-01-02::2018-12-31"
small_stk = as.matrix(stk[duration])
pl=matrix(0, nrow = nrow(small_stk), ncol = 1)
for(m in 2:length(rownames(small_stk)))

  
  







week=matrix(0, nrow = nrow(small_stk)-4, ncol = 1)
print(small_stk[1,"2330.TW.Open"])
print(small_stk[1,"2330.TW.Close"])
for(m in 1:nrow(pl)){
  pl[m,1]=small_stk[m,"2330.TW.Open"]-small_stk[m+4,"2330.TW.Close"]
  
}
head(pl,30)
head(Op(small_stk),30)
head(Cl(small_stk),30)
cpl=cumsum(pl)
plot(cpl,col="red",lwd=3,type='l',xaxt='n')
axis(1,1:length(to.weekly(rownames(small_stk))),to.weekly(rownames(small_stk)))

head(cbind(Op(stk[duration]),Cl(stk[duration])),10)
head(cbind(Cl(stk[duration]),Cl(stk[duration])-Op(stk[duration])),10)

pl=na.omit(Cl(stk[duration2])-Op(stk[duration]))
cpl=cumsum(pl)
plot(cpl)

STK = as.matrix(stk[duration])
LastC=Cl(STK)[1]

for(m in 2:length(time(STK))){
  if(Cl(STK)[m]>LastC){profit}
  
}
