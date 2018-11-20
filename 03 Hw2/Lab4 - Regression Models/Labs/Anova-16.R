## X1 is customer type, 3 different types of costumers (1=less than a year, 2=between 1 and 5 years, 3=longer than 5 years).
## X20 is likelihood of recommendation. We want to know if there are differences
## in X20 across the groups
## package ggplot
boxplot(hbat$X20~hbat$X1)
ggplot(hbat, aes(x=X1, y=X20,colour=X1))+geom_boxplot()+geom_jitter()

anov1=lm(X20~factor(X1), data=hbat)
summary(anov1)
# package dplyr
hbat %>% group_by(X1) %>% summarise(mean(X20))
par(mfrow=c(2,2))
plot(anov1)
par(mfrow=c(1,1))
## ANOVA hypothesis: normality, constant variance between groups, independence of observations.
## testing Normality in the groups
 shapiro.test(hbat$X20[hbat$X1==1])
 shapiro.test(hbat$X20[hbat$X1==2])
 shapiro.test(hbat$X20[hbat$X1==3])
## or testing normality through the residuals
shapiro.test(anov1$residuals)

## Testing homogeneity of variances, very sensitive to non normality
bartlett.test(hbat$X20~hbat$X1)
## residuals independence, package lmtest
dwtest(anov1, alternative="two.sided")
##  Welch test, in case of different variances
oneway.test(X20~factor(X1)-1, data=hbat)

## Anova result on this lm model, tells us that there are differences in the mean values of the groups, but it doesn't tell anything about
## which groups are different. We colud try a series of t.test() but they don't control for family wise error rate. If we don't control
# this error, the probability of rejecting (one or some) H_0's being true is higher than 0.05 (that is, the probability of incorrectly stating that there are differences between groups is not controlled). 
hbatt=data.frame(X1=hbat$X1, X20=hbat$X21)
 t.test(hbatt$X20[hbatt$X1==2],hbatt$X20[hbatt$X1==3])
 t.test(hbatt$X20[hbatt$X1==2],hbatt$X20[hbatt$X1==1])
 t.test(hbatt$X20[hbatt$X1==3],hbatt$X20[hbatt$X1==1])
#We find significant differences in 1 and 2, and, 1 and 3. But this is not the way to proceed when we are making many 
#comparison of this type. We have to adjust p-values.
##Using the function aov, it permits us to do that.

## 
anova2=aov(X20~X1, data=hbat)
## Once we have model anova2, fitted with the function aov(), we can use Levene test to test constant variances. It is less
# sensitive to departures from normality, package car.
leveneTest(anova2)

summary(anova2)
# General linear hypothesis and multiple comparisons for parametric models, package multcomp
## setting all pairwise comparisons with option "mcp(X1="Tukey")"
cht=glht(anova2,linfct=mcp(X1="Tukey"))
## adjusting the p-values. If no option is included, TukeyHSD method is used.
summary(cht, test =adjusted("bonferroni"))
par(mar=c(5.1,8,4.1,2.1))
plot(cht)
confint(cht)
cld(summary(cht,test=adjusted("bonferroni")))
plot(cld(summary(cht,test=adjusted("bonferroni"))))
# what happens if we adjust with the "holm" method?
cld(summary(cht, test =adjusted("holm")))
par(mar=c(5.1,5.1,5.1,2.1))
plot(cld(summary(cht, test =adjusted("holm"))))



## X22 is the percentage of purchases from hbat
anova3=aov(X22~X1, data=hbat)
par(oma=c(0,0,1,0))
cht2=glht(anova3,linfct=mcp(X1="Tukey"))
plot(cld(summary(cht2, test=adjusted(type="holm"))))

#An example with 5 groups
#competition=read.table("competition.txt", header=TRUE)
boxplot(competition$biomass~competition$clipping)
anova4=aov(biomass~clipping, data=competition) 
leveneTest(anova4) #equal variances in each group
summary(anova4)
pr=summary(glht(anova4,linfct=mcp(clipping="Tukey"), test=adjusted("holm")))
cld(pr)
plot(cld(pr))

# Assume we only wanted to test a priori if the mean of groups $n25$ and $n50$ is the same as the mean
# of groups $r10$ and $r5$ and if the mean of control and $n25$ equals the mean of the other three groups:

K=rbind("First test"=c(0,1,1,-1,-1), "Second-test"=c(1,1,-2/3,-2/3,-2/3) )
summary(glht(anova4, linfct=mcp(clipping=K), test=adjusted("holm")))

## Ancova with data perf
perf$T=factor(perf$T)
boxplot(perf$Y~perf$T)
boxplot(perf$S~perf$T)
plot(perf$S, perf$Y)
plot(perf$S, perf$Y, col=perf$T)
ancova3=lm(perf$Y~perf$S*perf$T) #model with interactions, different slopes for regression lines
summary(ancova3)

ggplot(perf, aes(x=S, y=Y, colour=factor(T)))+geom_point()+geom_smooth(method="lm")

#packages lmtest, gvlma
bptest(ancova3)
glvma(ancova3)

#comparing nested models, one with three parallel lines and the one with different slopes
anova(lm(perf$Y~perf$S+perf$T), ancova3)
# we should use the one with different slopes, the extra number of parameters (2) is worth 
# the reduction in the residual sum of squares.

#### How to relevel a factor:
perf$T=relevel(perf$T, ref="2")
str(perf)
