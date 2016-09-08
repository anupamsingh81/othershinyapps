
# Prior Calculation

x =0
y = 0.4
ORlow = 1- y
ORhigh =1- x

a= c(log(ORlow))
b= c(log(ORhigh))
CI = c(a,b)

# calculate mean and sd from 95% ci of logOR
LORlow = x - 1.96* y
LORhigh = x + 1.96*y
A =  matrix(data = c(1,-1.96,1,1.96),nrow =2,ncol=2,byrow =TRUE)
B = matrix (data = CI,nrow =2,ncol=1,byrow =FALSE)
round(solve(A,B),2)
G= round(solve(A,B),2)
equivN = 4/G[2]^2
equivN

priorparams = c(G[1],G[2],equivN)

# real data
logoddrat <- function(
  a1,b1=N1-a1,N1=a1+b1,
  a2,b2=N2-a2,N2=a2+b2) {
  log(((a1+.5)/(b1+.5))/
        ((a2+.5)/(b2+.5)))
}
sdlogoddrat <- function(
  a1,b1=N1-a1,N1=a1+b1,
  a2,b2=N2-a2,N2=a2+b2) {
  sqrt(1/(a1+.5)+1/(b1+.5) +1
       /(a2+.5)+1/(b2+.5))
}
logoddrat(8,1120,1128,92,1023,1145)
ORstudy = logoddrat(8,1120,1128,92,1023,1145)
sdlogoddrat(8,1120,1128,92,1023,1145)
sdstudy = sdlogoddrat(8,1120,1128,92,1023,1145)
equivM = 4/sdstudy^2
equivN = 4/sdstudy^2

realparams = c(ORstudy,sdstudy,equivM)
priorparams = c(ORstudy,sdstudy,equivN)

#new study
ORstudy2 = logoddrat(70,1042,1112,135,960,1095)

sdstudy2 = sdlogoddrat(70,1042,1112,135,960,1095)
equivM = 4/sdstudy2^2

realparams = c(ORstudy2,sdstudy2,equivM)

lastlogOR = (priorparams[3]*priorparams[1]+realparams[3]*realparams[1])/(priorparams[3]+realparams[3])
lastlogOR
lastSD = 2/sqrt(equivN+equivM)
lastSD
lastlogORCIup=lastlogOR+1.96*lastSD
lastlogORCIlow=lastlogOR - 1.96*lastSD
m  = exp(lastlogORCIup)
n = exp(lastlogORCIlow)
finalOR = c(m,n)
library(ClinicalRobustPriors)