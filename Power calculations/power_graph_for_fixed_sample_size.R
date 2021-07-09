# fixed sample size.
# known 
library(pwr)

# total number of lifetime mmbers = 12000
# conversion rate = 20%
# number of referred people = 2400
# each batch = 800

n = 80
vec = seq(10, 70, 10)
power_vec = numeric(0)
for(i in vec){

  n1=i
  n2=n-n1
  power_lis <- pwr.2p2n.test(h = ES.h(0.004,0.01), n1=n1, sig.level = 0.05, n2=n2, alternative ='less')
  power_vec <- c(power_vec, power_lis$power)
}

plot(vec, power_vec)


pwr.2p2n.test(h = ES.h(0.4178,0.3738), n1=225, n2=300, sig.level = 0.05, alternative ='two.sided')


prop.test(x= c(174,156), n=c(345, 346))
prop.test(x= c(27,10), n=c(345, 346))
