
##loading data
library(gdata)
library(downloader)
w="svilen.data_score.scv"
w_ch="svilen.data_chin.csv"
w_sq="svilen.data_squat.csv"
w_b="svilen.data_bench.csv"
download("https://raw.githubusercontent.com/ValentinPanayotov/skerlev_judo_project/master/svilen.data_score.csv", w)
download("https://raw.githubusercontent.com/ValentinPanayotov/skerlev_judo_project/master/svilen.data_bench.csv", w_b)
download("https://raw.githubusercontent.com/ValentinPanayotov/skerlev_judo_project/master/svilen.data_chin.csv", w_ch)
download("https://raw.githubusercontent.com/ValentinPanayotov/skerlev_judo_project/master/svilen.data_squat.csv", w_sq)




##splitting data
Bbeforeafter=filter(w_b, силова.подготовка=="1")
BbeforeafterO=filter(w_b, силова.подготовка=="0")
BbeforeafterM=filter(men_b, силова.подготовка=="1")
BbeforeafterM0=filter(men_b, силова.подготовка=="0")
BbeforeafterW=filter(women_b, силова.подготовка=="1")
BbeforeafterW0=filter(women_b, силова.подготовка=="0")

fitTLB0=lm(лег ~ преди.след ,data = BbeforeafterO)
fitTLBmen=lm(лег ~ преди.след ,data = Bbeforeafter)
fitBmen=lm(лег ~ преди.след ,data = BbeforeafterM)
fitBmen0=lm(лег ~ преди.след ,data = BbeforeafterM0)
fitBwomen=lm(лег ~ преди.след ,data = BbeforeafterW)
fitBwomen0=lm(лег ~ преди.след ,data = BbeforeafterW0)
fitTLmen= lm(оценка ~ силова.подготовка,data = men)              
fitTLwomen= lm(оценка ~ силова.подготовка,data = women)
Bmenbefore=split(w_b, w_b$преди.след)
summary(fitBwomen0)
summary(fitTCH)

##modeling matrix
library(rafalib)
library(dplyr)

Z=model.matrix( ~ силова.подготовка + пол + сил.трен:пол, data=w)
colnames(Z)
imagemat(Z)
## linear models exercises
fitTLB= lm(лег ~ силова.подготовка+ пол,data = w_b)
fitTLSQ= lm(клек ~ силова.подготовка+ пол,data = w_sq)
fitTCH= lm(набиране ~ силова.подготовка+ пол,data = w_ch)
fitTLBmen= lm(лег ~ силова.подготовка,data =Bmenbefore )
fitBmen= lm(лег ~ силова.подготовка,data =BbeforeafterM )
summary(fitBmen)






##filtering data

men<- filter(w,пол == "м")
women=filter(w,пол == "ж")
men
men_ch<- filter(w_ch,пол == "м")
women_ch=filter(w_ch,пол == "ж")

men_sq<- filter(w_sq,пол == "м")
women_sq=filter(w_sq,пол == "ж")
men_b<- filter(w_b,пол == "м")
women_b=filter(w_b,пол == "ж")


## Multiple regression men train after
scoreM1=filter(men, силова.подготовка=="1", преди.след=="after")
benchM1=filter(men_b, силова.подготовка=="1", преди.след=="after")
squatM1=filter(men_sq, силова.подготовка=="1", преди.след=="after")
chinM1=filter(men_ch, силова.подготовка=="1", преди.след=="after")
summary(benchM1)
summary(chinM1)
datatotalM=data.frame(scoreM1$оценка, chinM1$набиране, squatM1$клек, benchM1$лег)
multregM1=lm(scoreM1.оценка~chinM1.набиране + squatM1.клек + benchM1.лег, data= datatotalM)
summary(multregM1)
multregM1Inter=lm(scoreM1.оценка~chinM1.набиране * benchM1.лег + squatM1.клек, data= datatotalM)
summary(multregM1Inter)
##boxplot men
scoreTOT1=filter(w, силова.подготовка=="1")
boxplot(scoreTOT1$оценка~scoreTOT1$пол*scoreTOT1$преди.след, main="експериментална гр", col=c("grey90","grey40"), las=2)
scoreTOT0=filter(w, силова.подготовка=="0")
boxplot(scoreTOT0$оценка~scoreTOT0$пол*scoreTOT0$преди.след, main="контролна гр", col=c("grey90","grey40"), las=2)
mypar(1,2)
benchTOT1=filter(w_b, силова.подготовка=="1")
benchTOT0=filter(w_b, силова.подготовка=="0")
boxplot(benchTOT1$лег~benchTOT1$пол*benchTOT1$преди.след, main="експериментална гр", col=c("grey90","grey40"), las=2)
boxplot(benchTOT0$лег~benchTOT0$пол*benchTOT0$преди.след, "контролна гр", col=c("grey90","grey40"), las=2)
squatTOT1=filter(w_sq, силова.подготовка=="1")
squatTOT0=filter(w_sq, силова.подготовка=="0")
boxplot(squatTOT1$клек~squatTOT1$пол*squatTOT1$преди.след, main="експериментална гр",col=c("grey90","grey40"), las=2)
boxplot(squatTOT0$клек~squatTOT0$пол*squatTOT0$преди.след, main="контролна гр", col=c("grey90","grey40"), las=2)
chinTOT1=filter(w_ch, силова.подготовка=="1")
chinTOT0=filter(w_ch, силова.подготовка=="0")
boxplot(chinTOT1$набиране~chinTOT1$пол*chinTOT1$преди.след, main="експериментална гр", col=c("grey90","grey40"), las=2)
boxplot(chinTOT0$набиране~chinTOT0$пол*chinTOT0$преди.след, main="контролна гр", col=c("grey90","grey40"), las=2)



## Multiple regression men train before
scoreM0=filter(men, силова.подготовка=="1", преди.след=="before")
benchM0=filter(men_b, силова.подготовка=="1", преди.след=="before")
squatM0=filter(men_sq, силова.подготовка=="1", преди.след=="before")
chinM0=filter(men_ch, силова.подготовка=="1", преди.след=="before")
summary(benchM0)
summary(chinM0)
sd(chinM1$набиране)
datatotalM0=data.frame(scoreM0$оценка, chinM0$набиране, squatM0$клек, benchM0$лег)
multregM0=lm(scoreM0.оценка~chinM0.набиране + squatM0.клек + benchM0.лег, data= datatotalM0)
summary(multregM0)
## Multiple regression men no-train after
scoreM1n=filter(men, силова.подготовка=="0", преди.след=="after")
benchM1n=filter(men_b, силова.подготовка=="0", преди.след=="after")
squatM1n=filter(men_sq, силова.подготовка=="0", преди.след=="after")
chinM1n=filter(men_ch, силова.подготовка=="0", преди.след=="after")
summary(benchM1n)
summary(chinM1n)
summary(squatM1n)
datatotalMn=data.frame(scoreM1n$оценка, chinM1n$набиране, squatM1n$клек, benchM1n$лег)
multregM1n=lm(scoreM1n.оценка ~ chinM1n.набиране + squatM1n.клек + benchM1n.лег, data= datatotalMn)
summary(multregM1n)
## Multiple regression men no train before
scoreM0n=filter(men, силова.подготовка=="0", преди.след=="before")
benchM0n=filter(men_b, силова.подготовка=="0", преди.след=="before")
squatM0n=filter(men_sq, силова.подготовка=="0", преди.след=="before")
chinM0n=filter(men_ch, силова.подготовка=="0", преди.след=="before")
summary(benchM0n)
summary(chinM0n)
summary(squatM0n)
datatotalM0n=data.frame(scoreM0n$оценка, chinM0n$набиране, squatM0n$клек, benchM0n$лег)
multregM0n=lm(scoreM0n.оценка~chinM0n.набиране + squatM0n.клек + benchM0n.лег, data= datatotalM0n)
summary(multregM0n)

## Multiple regression women train after
scoreW1=filter(women, силова.подготовка=="1", преди.след=="after")
benchW1=filter(women_b, силова.подготовка=="1", преди.след=="after")
squatW1=filter(women_sq, силова.подготовка=="1", преди.след=="after")
chinW1=filter(women_ch, силова.подготовка=="1", преди.след=="after")

datatotalW=data.frame(scoreW1$оценка, chinW1$набиране, squatW1$клек, benchW1$лег)
multregW1=lm(scoreW1.оценка~chinW1.набиране + squatW1.клек + benchW1.лег, data= datatotalW)
summary(multregW1)
multregW1Inter=lm(scoreW1.оценка~chinW1.набиране*benchW1.лег + squatW1.клек, data= datatotalW)
summary(multregW1Inter)
summary(benchW1)
summary(chinW1)
## Multiple regression women train before
scoreW0=filter(women, силова.подготовка=="1", преди.след=="before")
benchW0=filter(women_b, силова.подготовка=="1", преди.след=="before")
squatW0=filter(women_sq, силова.подготовка=="1", преди.след=="before")
chinW0=filter(women_ch, силова.подготовка=="1", преди.след=="before")
summary(benchW0)
summary(chinW0)
summary(squatW1)
datatotalW0=data.frame(scoreW0$оценка, chinW0$набиране, squatW0$клек, benchW0$лег)
multregW0=lm(scoreW0.оценка~chinW0.набиране + squatW0.клек + benchW0.лег, data= datatotalW0)
summary(multregW0)
## Multiple regression women no-train after
scoreW1n=filter(women, силова.подготовка=="0", преди.след=="after")
benchW1n=filter(women_b, силова.подготовка=="0", преди.след=="after")
squatW1n=filter(women_sq, силова.подготовка=="0", преди.след=="after")
chinW1n=filter(women_ch, силова.подготовка=="0", преди.след=="after")
summary(benchW1n)
summary(chinW1n)
summary(squatW1n)
datatotalWn=data.frame(scoreW1n$оценка, chinW1n$набиране, squatW1n$клек, benchW1n$лег)
multregW1n=lm(scoreW1n.оценка ~ chinW1n.набиране + squatW1n.клек + benchW1n.лег, data= datatotalWn)
summary(multregW1n)
mypar(2,2)
plot(multregM1)
## Multiple regression women no-train before
scoreW0n=filter(women, силова.подготовка=="0", преди.след=="before")
benchW0n=filter(women_b, силова.подготовка=="0", преди.след=="before")
squatW0n=filter(women_sq, силова.подготовка=="0", преди.след=="before")
chinW0n=filter(women_ch, силова.подготовка=="0", преди.след=="before")
summary(benchW0n)
summary(chinW0n)
summary(squatW0n)
summary(scoreW1n)

datatotalW0n=data.frame(scoreW0n$оценка, chinW0n$набиране, squatW0n$клек, benchW0n$лег)
multregW0n=lm(scoreW0n.оценка~chinW0n.набиране + squatW0n.клек + benchW0n.лег, data= datatotalW0n)
summary(multregW0n)

## ANOVA men
wsex=split(w, w$пол)
w_chsex=split(w_ch, w_ch$пол)
w_bsex=split(w_b, w_b$пол)
w_sqsex=split(w_sq, w_sq$пол)
menBeforeScore=filter(wsex$м, преди.след=="before")
menBeforeChin=filter(w_chsex$м, преди.след=="before")
menBeforeBench=filter(w_bsex$м, преди.след=="before")
menBeforeSquat=filter(w_sqsex$м, преди.след=="before")
datatotalFilterM=data.frame(menBeforeScore$оценка, menBeforeChin$набиране ,menBeforeBench$лег, menBeforeSquat$клек )
datatotalFilterM$group=c(rep(1, 8),rep(0, 8))
fitM <- aov(group ~ menBeforeScore.оценка + menBeforeChin.набиране +  menBeforeBench.лег + menBeforeSquat.клек, data=datatotalFilterM)
summary(fitM)
menAfterScore=filter(wsex$м, преди.след=="after")
menAfterChin=filter(w_chsex$м, преди.след=="after")
menAfterBench=filter(w_bsex$м, преди.след=="after")
menAfterSquat=filter(w_sqsex$м, преди.след=="after")

datatotalFilterM0=data.frame(menBeforeScore$оценка, menBeforeChin$набиране ,menBeforeBench$лег, menBeforeSquat$клек )

datatotalFilterM0$group=c(rep(1, 8),rep(0, 8))
fitM0 <- aov(group ~ menAfterScore$оценка + menAfterChin$набиране +  menAfterBench$лег + menAfterSquat$клек, data=datatotalFilterM0)
summary(fitM0)


## ANOVA women
womenBeforeScore=filter(wsex$ж, преди.след=="before")
womenBeforeChin=filter(w_chsex$ж, преди.след=="before")
womenBeforeBench=filter(w_bsex$ж, преди.след=="before")
womenBeforeSquat=filter(w_sqsex$ж, преди.след=="before")

datatotalFilterW=data.frame(womenBeforeScore$оценка, womenBeforeChin$набиране ,womenBeforeBench$лег, womenBeforeSquat$клек )

datatotalFilterW$group=c(rep(1, 8),rep(0, 8))
fit <- aov(group ~ womenBeforeScore.оценка + womenBeforeChin.набиране +  womenBeforeBench.лег + womenBeforeSquat.клек, data=datatotalFilterW)
summary(fit)

womenAfterScore=filter(wsex$ж, преди.след=="after")
womenAfterChin=filter(w_chsex$ж, преди.след=="after")
womenAfterBench=filter(w_bsex$ж, преди.след=="after")
womenAfterSquat=filter(w_sqsex$ж, преди.след=="after")

datatotalFilterW0=data.frame(womenAfterScore$оценка, womenAfterChin$набиране ,womenAfterBench$лег, womenAfterSquat$клек )

datatotalFilterW0$group=c(rep(1, 8),rep(0, 8))
fit0 <- aov(group ~ womenAfterScore$оценка + womenAfterChin$набиране +  womenAfterBench$лег + womenAfterSquat$клек, data=datatotalFilterW0)
summary(fit0)
plot(fit0)

datatotalFilterW$group=w$силова.подготовка
datatotalFilterW
boxplot(fit)

##qqnorm women
qqnorm(datatotalFilterW$w_chsex.ж.набиране)
qqnorm(datatotalFilterW$w_bsex.ж.лег)
qqnorm(datatotalFilterW$w_sqsex.ж.клек)
##qqnorm men
qqnorm(datatotalFilterM$w_chsex.м.набиране)
qqnorm(datatotalFilterM$w_bsex.м.лег)
qqnorm(datatotalFilterM$w_sqsex.м.клек)
wsex

hist(datatotalFilterM$w_chsex.м.набиране)

## t-tests after
t.test(scoreW1n$оценка, scoreW1$оценка)
t.test(chinW1n$набиране, chinW1$набиране)
t.test(squatW1n$клек, squatW1$клек)
t.test(benchW1n$лег, benchW1$лег)


t.test(scoreM1n$оценка, scoreM1$оценка)
t.test(chinM1n$набиране, chinM1$набиране)
t.test(squatM1n$клек, squatM1$клек)
t.test(benchM1n$лег, benchM1$лег)
