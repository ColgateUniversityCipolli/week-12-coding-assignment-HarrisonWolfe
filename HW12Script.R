library(VGAM)
library(tidyverse)

n = 30
a = 0
b = 4

early = c()
normal = c()
counter = 0


for(i in 1:10000){
  curr.sample = rlaplace(n,a,b)
  early.sample = curr.sample[1:20]
  
  normal = t.test(curr.sample, alternative = "greater")[["statistic"]][["t"]]
  early = t.test(early.sample, alternative = "greater")[["statistic"]][["t"]]

  if (early > qt(0.95, df = 19)) {
    counter = counter + 1
  } else {
    if (normal > qt(0.95, df = 29)) {
      counter = counter + 1
    }
  }
 
  
}

(typeierror = counter/10000)


n=15
t210 = c()
t102 = c()
t1010 = c()

for(i in 1:10000){
  curr.sample210 = rbeta(n,2,10)
  curr.sample102 = rbeta(n,10,2)
  curr.sample1010 = rbeta(n,10,10)
  
  t210[i] = t.test(curr.sample210, mu = 1/6, alternative = "less")[["statistic"]][["t"]]
  t102[i] = t.test(curr.sample102, mu = 5/6, alternative = "less")[["statistic"]][["t"]]
  t1010[i] = t.test(curr.sample1010, mu = 0.5, alternative = "less")[["statistic"]][["t"]]
  
}


length(which(t210 < qt(0.05,14)))/10000
length(which(t102 < qt(0.05,14)))/10000
length(which(t1010 < qt(0.05,14)))/10000





n=15
t210 = c()
t102 = c()
t1010 = c()

for(i in 1:10000){
  curr.sample210 = rbeta(n,2,10)
  curr.sample102 = rbeta(n,10,2)
  curr.sample1010 = rbeta(n,10,10)
  
  t210[i] = t.test(curr.sample210, mu = 1/6, alternative = "greater")[["statistic"]][["t"]]
  t102[i] = t.test(curr.sample102, mu = 5/6, alternative = "greater")[["statistic"]][["t"]]
  t1010[i] = t.test(curr.sample1010, mu = 0.5, alternative = "greater")[["statistic"]][["t"]]
  
}


length(which(t210 > qt(0.95,14)))/10000
length(which(t102 > qt(0.95,14)))/10000
length(which(t1010 > qt(0.95,14)))/10000





n=15
t210 = c()
t102 = c()
t1010 = c()

for(i in 1:10000){
  curr.sample210 = rbeta(n,2,10)
  curr.sample102 = rbeta(n,10,2)
  curr.sample1010 = rbeta(n,10,10)
  
  t210[i] = t.test(curr.sample210, mu = 1/6)[["statistic"]][["t"]]
  t102[i] = t.test(curr.sample102, mu = 5/6)[["statistic"]][["t"]]
  t1010[i] = t.test(curr.sample1010, mu = 0.5)[["statistic"]][["t"]]
  
}


length(which(t210 > qt(0.975,14) | t210 < qt(0.025,14)))/10000
length(which(t102 > qt(0.975,14) | t102 < qt(0.025,14)))/10000
length(which(t1010 > qt(0.975,14) | t1010 < qt(0.025,14)))/10000


