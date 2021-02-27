library(tidyverse)
datstu = read.csv("file:///Users/DXL/Desktop/Econ613/datstu.csv")
datjss = read.csv("file:///Users/DXL/Desktop/Econ613/datjss.csv")
datsss = read.csv("file:///Users/DXL/Desktop/Econ613/datsss.csv")

# Part 1

# Exercise 1 Missing Data

# Number of students
length(datstu[,1])

# Number of schools
length(unique(unlist(datstu[,5:10])))

# Number of programs
length(unique(unlist(datstu[,11:16])))

# Number of choices
df = rbind(setNames(datstu[,c(5,11)], c("schoolcode","choicepgm")),
             setNames(datstu[,c(6,12)], c("schoolcode","choicepgm")),
             setNames(datstu[,c(7,13)], c("schoolcode","choicepgm")),
             setNames(datstu[,c(8,14)], c("schoolcode","choicepgm")),
             setNames(datstu[,c(9,15)], c("schoolcode","choicepgm")),
             setNames(datstu[,c(10,16)], c("schoolcode","choicepgm")))
dim(df %>% group_by_all %>% summarise())[1]

# Missing test score
sum(is.na(datstu[,2]))

# Apply to the same school (different programs)
count = 0
num = 1:dim(datstu)[1]
for (i in num) {
  if(sum(duplicated(datstu[i,5:10]))>0)  count = count+1
}
print(count)

# Apply to less than 6 choices
count = 0
for (i in 1:dim(datstu)[1]) {
  if(sum(is.na(datstu[i,5:10]))>0)  count = count+1
}
print(count)

# Exercise 2 Data

rankindex = which(datstu[,18]<=6)
rankplace = datstu[rankindex,18]
ssscode = c()
choicepgm = c()
score = c()
jssname = c()
for (i in 1:length(rankplace)){
  ssscode = append(ssscode, datstu[rankindex[i],rankplace[i]+4])
  choicepgm = append(choicepgm, toString(datstu[rankindex[i], rankplace[i]+10]))
  score = append(score, datstu[rankindex[i],2])
  jssname = append(jssname, toString(datstu[rankindex[i],17]))
}
df1 = data.frame(ssscode, choicepgm, score, jssname, rankplace)
summary = df1 %>% group_by(ssscode, choicepgm) %>%
  summarise(cutoff = min(score), quality = mean(score), size = n())
ssscode = summary %>% pull(ssscode)
df2 = data.frame(ssscode)
df2$choicepgm = summary %>% pull(choicepgm)
df2$sssname = datsss[,2][match(df2[,1], datsss[,3])]
df2$sssdistrict = datsss[,4][match(df2[,1], datsss[,3])]
df2$ssslon = datsss[,5][match(df2[,1], datsss[,3])]
df2$ssslat = datsss[,6][match(df2[,1], datsss[,3])]
df2$cutoff = summary %>% pull(cutoff)
df2$quality = summary %>% pull(quality)
df2$size = summary %>% pull(size)
# df2 is the required school level dataset.
df2[1:10,]

# Exercise 3 Distance

jssname = unique(datjss[,2])
sssname = unique(datsss[,4])
jsslon = datjss[,3][match(jssname, datjss[,2])]
jsslat = datjss[,4][match(jssname, datjss[,2])]
ssslon = datsss[,5][match(sssname, datsss[,4])]
ssslat = datsss[,6][match(sssname, datsss[,4])]
dist = c()
jssandsss = c()
for (i in 1:length(jssname)){
  for (j in 1:length(sssname)){
    d = sqrt((69.172*(ssslon[j]-jsslon[i])*cos(jsslat[i]/57.3))^2
             +(69.172*(ssslat[j])-jsslat[i]))^2
    dist = append(dist, d)
    jssandsss = append(jssandsss, paste(toString(jssname[i]), "&",
                                          toString(sssname[j])))
  }
}
df3 = data.frame(jssandsss, dist)
# df3 is the required dataset for the distance between 
# junior high school and senior high school.
df3[1:10,]

# Exercise 4 Descriptive Characteristics

df4 = df1
df4$sssname = datsss[,4][match(df4[,1], datsss[,3])]
jssandsss = c()
for (i in 1:length(df4$jssname)){
  jssandsss = append(jssandsss, paste(toString(df4$jssname[i]), "&",
                                       toString(df4$sssname[i])))
}
df4$jssandsss = jssandsss
df4$dist = df3[,2][match(df4$jssandsss, df3[,1])]
summary1 = df4 %>% group_by(rankplace) %>%
  summarise(cutoff = min(score), qualitymean = mean(score),
            qualitysd = sd(score), distmean = mean(dist),
            distsd = sd(dist))
rankplace = summary1 %>% pull(rankplace)
df5 = data.frame(rankplace)
df5$cutoff = summary1 %>% pull(cutoff)
df5$qualitymean = summary1 %>% pull(qualitymean)
df5$qualitysd = summary1 %>% pull(qualitysd)
df5$distmean = summary1 %>% pull(distmean)
df5$distsd = summary1 %>% pull(distsd)
# df5 is the required dataset differentiating by ranked choice.
df5
summary2 = df4 %>%
  summarise(quantile = quantile(score, c(0.25, 0.5, 0.75)))
quantile = summary2 %>% pull(quantile)
scorequantile = c()
for (i in 1:length(df4$score)){
  if (df4$score[i]<=quantile[1]){
    scorequantile[i] = "25th"
  }
  else if (df4$score[i]>quantile[1] && df4$score[i]<=quantile[2]){
    scorequantile[i] = "25th-50th"
  }
  else if (df4$score[i]>quantile[2] && df4$score[i]<=quantile[3]){
    scorequantile[i] = "50th-75th"
  }
  else {
    scorequantile[i] = "75th-100th"
  }
}
df4$scorequantile = scorequantile
summary3 = df4 %>% group_by(scorequantile) %>%
  summarise(cutoff = min(score), qualitymean = mean(score),
            qualitysd = sd(score), distmean = mean(dist),
            distsd = sd(dist))
scorequantile = summary3 %>% pull(scorequantile)
df6 = data.frame(scorequantile)
df6$cutoff = summary3 %>% pull(cutoff)
df6$qualitymean = summary3 %>% pull(qualitymean)
df6$qualitysd = summary3 %>% pull(qualitysd)
df6$distmean = summary3 %>% pull(distmean)
df6$distsd = summary3 %>% pull(distsd)
# df6 is the required dataset differentiating by student test score quantiles.
df6

# Part 2 Data Creation

# Exercise 5 Data Creation

x1 = runif(10000,1,3)
x2 = rgamma(10000,shape=3,scale=2)
x3 = rbinom(10000,size=1,prob=0.3)
epsilon = rnorm(10000,2,1)
y = 0.5 + 1.2*x1 - 0.9*x2 + 0.1*x3 + epsilon
ydum = rep(0,length(y)) 
ydum[y > mean(y)] = 1

# Exercise 6 OLS

# The correlation between y and x1
cor(x1, y)
# The correlation between Y and X1 is 0.2, which has the same
# sign as 1.2.

# Creat matrices X and Y
x = as.matrix(cbind(x1, x2, x3))
intercept <- rep(1, nrow(x))
Y = as.matrix(y)
X = as.matrix(cbind(intercept, x))

# Calculate the coefficients on this regression
betas = solve(t(X) %*% X) %*% t(X) %*% Y
betas

# Calculate the standard errors using the standard formulas of the OLS
residuals = Y - X %*% betas
p = ncol(X) - 1 
df = nrow(X) - p - 1 
res_var = sum(residuals^2) / df 
beta_cov = res_var * solve(t(X) %*% X) 
beta_se = sqrt(diag(beta_cov))
beta_se

# Exercise 7 Discrete Choice

# Probit Model
probit = glm(ydum ~ x1 + x2 + x3, family = binomial(link = "probit"))
summary(probit)
# Both x1 and x3 increase the probability that ydum = 1, while x2
# decreases the probability that ydum = 1. Only x3 is not significant.

# Logit Model
logit = glm(ydum ~ x1 + x2 + x3, family = binomial(link = "logit"))
summary(logit)
# Both x1 and x3 increase the probability that ydum = 1, while x2
# decreases the probability that ydum = 1. Only x3 is not significant.

# Linear Model
linear = lm(y ~ x1 + x2 + x3)
summary(linear)
# A unit increase in x1 decreases y by 
summary(linear)$coefficients[2,1]
# A unit increase in x2 decreases y by
summary(linear)$coefficients[3,1]
# A unit increase in x3 increases y by
summary(linear)$coefficients[4,1]
# Only the coefficient is significant.

# Exercise 8 Marginal Effects

# Probit Model
# The marginal effect of x1 is
summary(probit)$coefficients[2,1]
# The standard error of the marginal effect of x1 is
summary(probit)$coefficients[2,2]
# The marginal effect of x2 is
summary(probit)$coefficients[3,1]
# The standard error of the marginal effect of x2 is
summary(probit)$coefficients[3,2]
# The marginal effect of x3 is
summary(probit)$coefficients[4,1]
# The standard error of the marginal effect of x3 is
summary(probit)$coefficients[4,2]

# Logit Model
# The marginal effect of x1 is
summary(logit)$coefficients[2,1]
# The standard error of the marginal effect of x1 is
summary(logit)$coefficients[2,2]
# The marginal effect of x2 is
summary(logit)$coefficients[3,1]
# The standard error of the marginal effect of x2 is
summary(logit)$coefficients[3,2]
# The marginal effect of x3 is
summary(logit)$coefficients[4,1]
# The standard error of the marginal effect of x3 is
summary(logit)$coefficients[4,2]











