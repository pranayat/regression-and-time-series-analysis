#Jascha Alexander & Carmen Mezquita

dat = read.table("mygardens.RData",header=T)

m = mean(dat[,1])
mv = matrix(m,20,1)

gardenA = matrix(0,10,2)
gardenB = matrix(0,10,2)

j = 1
k = 1

for (i in 1:20) {
  if (i%%2 == 1){
    gardenA[j,] = c(i,dat[i,1])
    j = j+1
  }
  else{
    gardenB[k,] = c(i,dat[i,1])
    k = k+1
  }
}

mA = mean(gardenA[,2])
mB = mean(gardenB[,2])
mAv = matrix(mA,20,1)
mBv = matrix(mB,20,1)

par(mfrow=c(2,1))

plot(1:20,dat$ozone, pch=19, xlab="samples", ylab="ozone")
lines(1:20,mv,col='red')
for (i in 1:20) {
  lines(c(i,i),c(dat[i,1],4))
}

plot(gardenA[,1],gardenA[,2], pch=19, xlab="samples", ylab="ozone",ylim = c(1,7),xlim = c(1,20))
points(gardenB[,1],gardenB[,2], pch=19, col="red")
lines(1:20,mAv)
lines(1:20,mBv,col='red')

for (i in 1:10) {
  lines(c(2*i-1,2*i-1),c(dat[2*i-1,1],3))
  lines(c(2*i,2*i),c(dat[2*i,1],5),col = 'red')
}

tsos = sum((dat[,1]-4)^2)
Asos = sum((gardenA[,2]-3)^2)
Bsos = sum((gardenB[,2]-5)^2)
Dihat = sum((dat[,1]-3)^2)+sum((dat[,1]-5)^2)
R = 18*(20/Dihat)

if (R<qf(0.95,2,18)) {
  print('no significant difference between A and B')
}

print(tsos)
print(Asos)
print(Bsos)
print(R)

anova = aov(ozone~garden, data = dat)
print(summary.aov(anova))



