
require(knitr)
require(markdown)
require(pander)
require(formatR)
require(highlight)
require(xtable)
require(MASS)
require(ElemStatLearn)
require(HSAUR3)
require(foreign)
require(xlsx)
require(lattice)
require(reshape2)
require(ggplot2)
require(plyr)
require(dplyr)
require(data.table)
require(tidyr)
require(lattice)
require(latticeExtra)
require(latticeDensity)
require(latticeKrig)
require(directlabels)
require(mangoTraining)
require(mlmRev)
require(nlme)

setwd("C:/RData/")
load("dmfnfclass20160301.Rda")

#Question1
xyplot(med_cost ~ age ,
     ylab="Total Medical Fee(NT dollars)",
     xlab="Age", 
     type = c("p", "g"),
     main="Q1-1:醫療花費的年齡分布圖",
     data = dmfnfclass20160301)
xyplot(log(med_cost) ~ age ,
     ylab="Logarithmic Total Medical Fee",
     xlab="Age",
     type = c("p", "g"),
     main="Q1-2:醫療花費的年齡分布圖",
     data = dmfnfclass20160301)

#Question2
xyplot(dmfailtime ~ age ,
     ylab="Dmfailtime",
     xlab="Age",
     main="Q2-1:Dmfailtime的年齡分布圖",
     type = c("p", "r"),
     col = c("orange"),
     col.line = "red",
     data = dmfnfclass20160301)

xyplot(dmfail ~ age ,
       ylab="Dmfail",
       xlab="Age",
       main="Q2-2:Dmfail的年齡分布圖",
       type = c("p", "r"),
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)

#Question3
xyplot(med_cost ~ dmfailtime ,
       ylab="Total Medical Fee(NT dollars)",
       xlab="Dmfailtime", 
       type = c("p", "r"),
       main="Q3-1:醫療花費的五年內治療失敗分布圖",
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)

xyplot(log(med_cost) ~ dmfailtime ,
       ylab="Logarithmic Total Medical Fee",
       xlab="Dmfailtime", 
       type = c("p", "r"),
       main="Q3-2:醫療花費的五年內治療失敗分布圖",
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)

#Question4
xyplot(log(med_cost) ~ hospvolume ,
       ylab="Logarithmic Total Medical Fee",
       xlab="hospvolume", 
       type = c("p", "r"),
       main="Q4-1:醫療花費的住院量分布圖",
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)
xyplot(dmfailtime ~ hospvolume ,
       ylab="Dmfailtime",
       xlab="hospvolume", 
       type = c("p", "r"),
       main="Q4-2:dmfailtime的住院量分布圖",
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)

#Question5
xyplot(log(med_cost) ~ bipolar ,
       ylab="Logarithmic Total Medical Fee",
       xlab="bipolar", 
       type = c("p", "r"),
       main="Q5-1:醫療花費的bipolar分布圖",
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)
xyplot(dmfailtime ~ bipolar ,
       ylab="dmfailtime",
       xlab="bipolar", 
       type = c("p", "r"),
       main="Q5-2:dmfailtime的bipolar分布圖",
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)

#Question6
xyplot(male ~ dmfail ,
       ylab="male",
       xlab="dmfail", 
       type = c("p", "r"),
       main="Q6-1:male的dmfail分布圖",
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)
xyplot(bipolar ~ dmfail ,
       ylab="bipolar",
       xlab="dmfail", 
       type = c("p", "r"),
       main="Q6-2:bipolar的dmfail分布圖",
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)
xyplot(cin4cat ~ dmfail ,
       ylab="cin4cat",
       xlab="dmfail", 
       type = c("p", "r"),
       main="Q6-3:cin4cat的dmfail分布圖",
       col = c("orange"),
       col.line = "red",
       data = dmfnfclass20160301)
xyplot(bipolar ~ dmfail,
       ylab="bipolar",
       xlab="dmfail", 
       groups = city5cat,
       data = dmfnfclass20160301,
       type = c("p", "r"),
       panel = function(x, y, ...)
         { panel.xyplot(x, y, ...)},
       main="Q6-4:對city5之bipolar的dmfail分布圖")
#Question7
xyplot(log(med_cost) ~ bipolar,
       ylab="log(med_cost)",
       xlab="bipolar", 
       groups = male,
       data = dmfnfclass20160301,
       type = c("p", "r"),
       panel = function(x, y, ...)
        { panel.xyplot(x, y, ...)},
       main="Q7-1:對male(1) female(0)之log(med_cost)的bipolar分布圖")
xyplot(dmfailtime ~ bipolar,
       ylab="dmfailtime",
       xlab="bipolar", 
       groups = male,
       data = dmfnfclass20160301,
       type = c("p", "r"),
       panel = function(x, y, ...)
       { panel.xyplot(x, y, ...)},
       main="Q7-2:對male(1) female(0)之dmfailtime的bipolar分布圖")
#Question8
cloud(med_cost ~ hospvolume * dmfailtime,
      data = dmfnfclass20160301,
      zlim = rev(range(dmfnfclass20160301$med_cost)),
      screen = list(z = 105, x = -70),
      panel.aspect = 0.75)
cloud(med_cost ~ cindex * dmfailtime,
      data = dmfnfclass20160301,
      zlim = rev(range(dmfnfclass20160301$med_cost)),
      screen = list(z = 105, x = -70),
      panel.aspect = 0.75)
