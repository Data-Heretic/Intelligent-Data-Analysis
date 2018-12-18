load("~/Documents/EIT Digital Masterschool/UPM/1st semester/Intelligent Data Analysis/Homeworks/1.1/mpg.RData")

library("tidyverse")
library("tseries")
library("ggridges")

mpg <- mpg[-which(mpg$fl=="c"),] # remove row containing strange value 'c' for the variable fl

mpg=mpg %>%
  mutate(
    trans_n = fct_collapse(
    trans,
    manual = c(trans[substr(trans,1,1)=="m"]),
    auto = c(trans[substr(trans,1,1)=="a"])
    ))


mpg=mpg %>% 
  mutate(
    fl_n = fct_lump(
      fl
    )
  )

mpg$year = as.factor(mpg$year)
mpg$drv = as.factor(mpg$drv)
mpg$cyl = as.factor(mpg$cyl)
mpg$fl = as.factor(mpg$fl)
mpg$class = as.factor(mpg$class)

boxplot(mpg$cty ~mpg$trans_n)
boxplot(mpg$cty ~mpg$year)
boxplot(mpg$cty ~mpg$drv)
boxplot(mpg$cty ~mpg$fl) # boxplot(mpg$cty ~mpg$fl_n)
boxplot(mpg$cty ~mpg$class)

var.test(mpg$cty ~mpg$trans_n) # Performs an F test to compare the variances of two samples from normal populations.
t.test(mpg$cty~mpg$trans_n, var.equal=TRUE)
jarque.bera.test(mpg$cty[mpg$trans_n=="auto"]) # Tests the null of normality for x using the Jarque-Bera test statistic.
jarque.bera.test(mpg$cty[mpg$trans_n=="manual"])
wilcox.test(mpg$cty~mpg$trans_n) # Non-parametric statistical hypothesis test, for the comparison of the means between 2 paired samples
kruskal.test(mpg$cty~mpg$trans_n) # Using the Kruskal-Wallis Test, we can decide whether the population distributions are identical without assuming them to follow the normal distribution.


boxplot(mpg$hwy ~mpg$trans_n)

var.test(mpg$hwy ~mpg$trans_n)
t.test(mpg$hwy~mpg$trans_n, var.equal=TRUE)
jarque.bera.test(mpg$hwy[mpg$trans_n=="auto"]) 
jarque.bera.test(mpg$hwy[mpg$trans_n=="manual"])
wilcox.test(mpg$hwy~mpg$trans_n) 

# one-way anova between hwy and trans_n
# Compute the analysis of variance
res.aov.hwy.trans_n <- aov(hwy ~ trans_n, data = mpg)
# Summary of the analysis
summary(res.aov.hwy.trans_n)
# As the ANOVA test is significant, we can compute Tukey HSD for performing multiple pairwise-comparison between the means of groups.
TukeyHSD(res.aov.hwy.trans_n)
# Check the homogeneity of variance assumption
plot(res.aov.hwy.trans_n, 1)
library(car)
leveneTest(hwy ~ trans_n, data = mpg)
# Check the normality assumption
plot(res.aov.hwy.trans_n, 2)
# The Shapiro-Wilk test on the ANOVA residuals finds that normality is violated.
aov_residuals.hwy.trans_n <- residuals(object = res.aov.hwy.trans_n ) # Extract the residuals
shapiro.test(x = aov_residuals.hwy.trans_n ) # Run Shapiro-Wilk test

# Just about every parametric statistical test has a non-parametric substitute, such as the Kruskal–Wallis test instead of a one-way anova, 
# Wilcoxon signed-rank test instead of a paired t-test, and Spearman rank correlation instead of linear regression. 
# These non-parametric tests do not assume that the data fit the normal distribution. 
# They do assume that the data in different groups have the same distribution as each other, however; 
# if different groups have different shaped distributions (for example, one is skewed to the left, another is skewed to the right), 
# a non-parametric test may not be any better than a parametric one.
library("moments")
skewness(mpg$hwy[which(mpg$trans_n=="auto")])
skewness(mpg$hwy[which(mpg$trans_n=="manual")])
# the skewness of the two distributions is quite similar so we can apply the Kruskal-Wallis test.
kruskal.test(mpg$hwy~mpg$trans_n)



# distribution of city mileage through two categorical variables
ggplot(data = mpg, aes(y=cty, x=trans_n, col=factor(year))) +
  geom_boxplot() 

# How does the city mileage distribution change over the years?
ggplot(mpg,aes(x=cty, y=year, fill=factor(..quantile..)))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE, scale=0.9)

# 
ggplot(mpg, aes(x = cty, fill = trans_n)) +
  geom_bar() +
  xlab("City Mileage in miles per gallon") +
  ylab("Total Count") +
  labs(fill = "type of transmission") 

# 
ggplot(mpg, aes(x = cty, y=..prop.., fill = trans_n, group=trans_n)) +
  geom_bar(position=position_dodge()) +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  xlab("City Mileage in miles per gallon") +
  ylab("Percentage") +
  labs(fill = "type of transmission") +
  scale_y_continuous(labels = scales::percent)




# Visualize the 3-way relationship of cty, trans_n and year
ggplot(mpg, aes(x = cty, fill = trans_n)) +
  stat_count(width = 0.5) +
  facet_wrap(~year) +
  ggtitle("Year") +
  xlab("City Mileage in miles per gallon") +
  ylab("Total Count") +
  labs(fill = "type of transmission")

# Take a look at City Mileage broken out by type of transmission, year of manufacturing and fuel type
ggplot(mpg, aes(x = cty, fill = trans_n)) +
  facet_wrap(~fl + year) +
  geom_histogram(binwidth = 10) +
  xlab("City Mileage in miles per gallon") +
  ylab("Total Count") +
  labs(fill = "type of transmission")

# More fine-grained
ggplot(mpg, aes(x = cty, fill = trans_n)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~year) +
  ggtitle("Year of manufacturing") +
  xlab("City Mileage in miles per gallon") +
  ylab("Total Count") +
  ylim(0,15) +
  labs(fill = "type of transmission")






table0=xtabs(~year+trans_n, data=mpg) # Create a contingency table (optionally a sparse matrix) from cross-classifying factors
prop.table(table0,1) # generates frequency tables
mosaicplot(table0,shade=TRUE, type="pearson",main="Survival on the Titanic")
chisq.test(table0) # here the p-value is computed from the asymptotic chi-squared distribution of the test statistic
chisq.test(table0, simulate.p.value=TRUE) # here the p-value is computed for a Monte Carlo test with B replicates

# mosaic plots using package vcd
library("vcd")
mosaic(table0)
mosaic(table0, gp=shading_max, split_vertical=TRUE)



# Let's try to define a similiarity (or dissimilarity) index among number of cylinders
cyl4=filter(mpg,cyl==4)
cyl4=cyl4[,c(3,8,9)]

cyl5=filter(mpg,cyl==5)
cyl5=cyl5[,c(3,8,9)]

cyl6=filter(mpg,cyl==6)
cyl6=cyl6[,c(3,8,9)]

cyl8=filter(mpg,cyl==8)
cyl8=cyl8[,c(3,8,9)]

CYLINDERS4=cyl4 %>% summarise_all(funs(median(.)))
CYLINDERS5=cyl5 %>% summarise_all(funs(median(.)))
CYLINDERS6=cyl6 %>% summarise_all(funs(median(.)))
CYLINDERS8=cyl8 %>% summarise_all(funs(median(.)))

CYLINDERS4=as.numeric(CYLINDERS4)
CYLINDERS5=as.numeric(CYLINDERS5)
CYLINDERS6=as.numeric(CYLINDERS6)
CYLINDERS8=as.numeric(CYLINDERS8)

library(factoextra)
df=rbind(CYLINDERS4,CYLINDERS5,CYLINDERS6,CYLINDERS8)
get_dist(df,method="euclidean")
fviz_dist(get_dist(df,method="euclidean")) #dissimilarity
fviz_dist(1-get_dist(df, method="euclidean")) #similarity


