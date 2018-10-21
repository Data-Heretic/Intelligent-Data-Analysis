pp <- read.csv("paris_paintings.csv", 
               na.strings = c("NA","n/a", "", "#NAME?", " ", "Unknown", "X"), 
               stringsAsFactors = FALSE)

dim(pp)
str(pp)
# pp1=as.data.frame(pp)

pp$price<- as.numeric(gsub(",","",pp$price))
?gsub

# Some useful commands
head(pp)
table(pp$year)
summary(pp$price)
hist(log(pp$price))
table(pp$year)
hist(pp$price)
boxplot(log(pp$price) ~pp$school_pntg)
boxplot(log(pp$price) ~pp$year)
boxplot(pp$price ~pp$school_pntg)
boxplot(log(pp$price) ~pp$school_pntg)
boxplot(log(pp$price) ~pp$Shape_nn)

##
sum(complete.cases(pp))

# Summarize (count) NA's per variable
t=pp %>%
  summarise_all(funs(sum(is.na(.))))

t
# Visualizing missing data
library("Amelia")
missmap(pp)

# Recoding shape of paintings

table(pp$Shape)

pp %>%
  group_by(Shape) %>%
  summarise(n = n()) %>%
  arrange(n)

pp=pp %>%
  mutate(
    Shape_n = fct_recode(
      Shape, 
      octagon="octogon", 
      oval="ovale", 
      round="ronde", 
      rect="squ_rect",
      NULL="")
  ) 

# Lumping shape of paintings (1)
pp=pp %>%
  mutate(
    Shape_nn = fct_lump(
      Shape)
  ) 

# Lumping shape of paintings (2)
pp %>%
  mutate(Shape = fct_recode(Shape, NULL="") %>% fct_lump())%>%
  group_by(Shape) %>%
  summarize(n=n()) %>%
  arrange(n)

# Now, we can explore if the price is higher for square or 
# rectangular paintings

boxplot(log(pp$price) ~pp$Shape_nn)
var.test(log(pp$price)[pp$Shape_nn=="squ_rect"],log(pp$price)[pp$Shape_nn=="Other"])
t.test(log(pp$price)[pp$Shape_nn=="squ_rect"],log(pp$price)[pp$Shape_nn=="Other"], var.equal=TRUE)
jarque.bera.test(na.omit(log(pp$price)[pp$Shape_nn=="squ_rect"]))
jarque.bera.test(na.omit(log(pp$price)[pp$Shape_nn=="Other"]))

wilcox.test(log(pp$price)~pp$Shape_nn)
kruskal.test(log(pp$price)~pp$Shape_nn)

# Recoding variable mat in a new variable mat_n

pp=pp %>%
  mutate(
    mat_n = fct_collapse(
      mat, 
      metal  = c("a", "bc", "br", "c"),
      stone  = c("al", "ar", "m"),
      canvas = c("co", "bt", "t","ta"),
      paper  = c("p", "ca"),
      wood   = c("b"),
      other  = c("o", "e", "v", "h","mi","pa","g"),
      NULL   = c("n/a", "")
    ))

sum(is.na(pp$mat_n))

# Is the mean price different through the different categories of these variables?
# One way anova
table(pp$mat_n)
table(pp$year)
table(pp$school_pntg)
table(pp$dealer)
table(pp$diff_origin)
table(pp$endbuyer)

# distribution of log(price) through one categorical variable 
ggplot(data = pp, aes(y=price, x=materialCat, col=materialCat)) +
  geom_boxplot() + 
  scale_y_continuous(breaks=c(10,1000,3000),trans = scales::log_trans(base=exp(1)))

# package ggridges
ggplot(pp,aes(x=logprice, y=materialCat, fill=factor(..quantile..)))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE, scale=0.9)

# distribution of log(price) through two categorical variables, three different ways to
# do the same plot.
# They can be complemented with interaction plots
ggplot(data = pp, aes(y=logprice, x=materialCat, col=factor(prevcoll))) +
  geom_boxplot() 

ggplot(data = pp, aes(y=price, x=materialCat, col=factor(prevcoll))) +
  geom_boxplot() + 
  scale_y_log10()

ggplot(data = pp, aes(y=price, x=materialCat, col=factor(prevcoll))) +
  geom_boxplot() + 
  scale_y_continuous(breaks=c(10,1000,3000),trans = scales::log_trans(base=exp(1)))

interaction.plot(pp$materialCat, pp$prevcoll, pp$logprice, fun=median,col=c("red","green"),pch=c(15,19), type="b", trace.label="previousow")
#  When the previous owner is known, paintings are sold for a higher price.
# (Marginal effect of previous owner). There doesn't seem to be an interaction
# between the categorical variables on the price of the paintings. What
# about the marginal effect of material?

# Quantitative var =logprice, categoprical variables: diff_origin, endbuyer
table(pp$endbuyer,pp$diff_origin)

ggplot(data = pp, aes(y=price, x=endbuyer, col=factor(diff_origin))) +
  geom_boxplot() + 
  scale_y_continuous(breaks=c(10,1000,3000),trans = scales::log_trans(base=exp(1)))

# interaction plot: fun=mean, median.
# The effect of diff_origin is consistent through all 5 endbuyers. This suggests
# that there is not an interaction effect. 
interaction.plot(pp$endbuyer, pp$diff_origin, pp$logprice, fun=median,col=c("red","green"),pch=c(15,19), type="b", trace.label="DiffOrigin")

# You can try to improve the previous plot adding confidence intervals to each mean value

# You can try an interaction plot with dealer and diff_origin
interaction.plot(pp$dealer, pp$diff_origin, pp$logprice, fun=median,col=c("red","green"),pch=c(15,19), type="b", trace.label="DiffOrigin")
ggplot(data = pp, aes(y=price, x=dealer, col=factor(diff_origin))) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(10,1000,3000),trans = scales::log_trans(base=exp(1)))

# How does the price distribution change over the years?
# compare these plots
ggplot(pp,aes(x=price, y=as.factor(year)))+
  geom_density_ridges(scale=0.9)

ggplot(data = pp, aes(x=logprice, y=as.factor(year))) +
  geom_density_ridges(scale=0.9)

ggplot(pp,aes(x=logprice, y=as.factor(year), fill=factor(..quantile..)))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE, scale=0.9)

ggplot(pp, aes(x=logprice, y=as.factor(year))) +
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 1, alpha = 0.7, scale=0.7)

# Let's explore the relationship between logprice and Surface 
# (of painting in square inches)
summary(pp$Surface)
which(pp$Surface==0)
ind=which(pp$Surface==0)
pp[ind,]
pp_surf=pp[-ind,]
pp_surf=as.data.frame(pp_surf)

# Let's try lo linearize the relationship between both variables
plot(pp_surf$Surface~pp_surf$logprice)
pp_surf$logsurf=log(pp_surf$Surface)
plot(pp_surf$logsurf,pp_surf$logprice)

# Linear correlations by year between logprice and logsurface
pp_surf %>% 
  group_by(year) %>%
  summarise(n(), min(price), max(price))

corr_year=pp_surf %>% 
  group_by(year) %>%
  do(as.data.frame(cor(cbind(.$logprice,.$logsurf),use="complete.obs")))

corr_year=pp_surf %>% 
  group_by(year) %>%
  do(as.data.frame(cor(cbind(.$logprice,.$logsurf),use="complete.obs")[2,1]))

colnames(corr_year)=c("year","correlation")
corr_year=as.data.frame(corr_year)

ggplot(data = corr_year, aes(x = year, y = correlation))+
  geom_line(color = "#00AFBB", size = 1)+scale_x_discrete(limits=c(1764:1780))

# Material and price (for finished paintings and for unfinished paintings). 
ggplot(pp_surf, aes(x=Surface,y=logprice, color=factor(materialCat)))+geom_point()+geom_smooth(method="lm")

ggplot(pp_surf[pp_surf$finished=="1",], aes(x=logsurf,y=logprice, color=factor(materialCat)))+geom_point()+geom_smooth(method="lm")

ggplot(pp_surf[pp_surf$finished=="0",], aes(x=logsurf,y=logprice, color=factor(materialCat)))+geom_point()+geom_smooth(method="lm")

# Shape and price.
ggplot(pp_surf, aes(x=logsurf,y=logprice, color=factor(Shape_nn)))+geom_point()+geom_smooth(method="lm")

# Let's try to define a similiarity (or dissimilarity) index among schools of paintings
D=filter(pp,school_pntg=="D/FL" )
D=D[,c(16,34:60)]

SPA=filter(pp,school_pntg=="S" )
SPA=SPA[,c(16,34:60)]

FRA=filter(pp,school_pntg=="F" )
FRA=FRA[,c(16,34:60)]

IT=filter(pp,school_pntg=="I" )
IT=IT[,c(16,34:60)]

SPAIN=SPA %>% summarise_all(funs(median(.)))
DUTCH=D %>% summarise_all(funs(median(.)))
FRANCE=FRA %>% summarise_all(funs(median(.)))
ITALY=IT %>% summarise_all(funs(median(.)))
SPAIN=as.numeric(SPAIN)
DUTCH=as.numeric(DUTCH)
FRANCE=as.numeric(FRANCE)
ITALY=as.numeric(ITALY)

df=rbind(SPAIN,DUTCH,FRANCE,ITALY)
dist(df, method="binary")
1-dist(df, method="binary")
library(factoextra)
get_dist(df,method="binary")
fviz_dist(get_dist(df,method="binary")) #dissimilarity
fviz_dist(1-get_dist(df, method="binary")) #similarity

# More data manipulation examples
select(pp, origin_author:winningbiddertype) %>% 
  filter(winningbiddertype == "D") %>%
  group_by(origin_author) %>%
  summarise(n = n(), n_school_pntg = n_distinct(school_pntg))

prueba=select(pp, 7:10) %>% 
  mutate(price_cat = ifelse(pp$price < median(pp$price), "lt median", "gt median"))

# Once you know the variables you want to work with,
# you subset them such that there are no NAs in any of these variables. This is an example
# of how to do that:
pp_sub = filter(pp, !is.na(school_pntg), !is.na(endbuyer), 
                !is.na(Interm), !is.na(price), 
                !is.na(Shape), !is.na(mytho), 
                !is.na(peasant), !is.na(lrgfont))
dim(pp_sub)