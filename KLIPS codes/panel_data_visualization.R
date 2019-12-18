# Mater's thesis
# A Study on Labor Experience of First Generation College Students using Korean Panel Data 
# : The Challenges and Privileges
# Modified 191207 by JISU (ver.2)
# Modified 191215 by JISU (ver.3)


library(stargazer)


visu.data <- data.frame("SEX" = data$sex, "AGE" = data$age, "EDU" = data$eduyear, 
                        "REGULAR" = data$regular, "SERVICE" = data$serviceyear, "WORK" = data$workyear, "SIZE" = data$size,
                        "WAGE" = data$annual.posttax, "FirstGen" = data$first.gen)
visu.data2 <- visu.data[visu.data$FirstGen == 1,-ncol(visu.data)]
visu.data3 <- visu.data[visu.data$FirstGen == 0,-ncol(visu.data)]

stargazer(visu.data, summary.stat = c("n", "mean", "sd", "min", "max"), title="Descriptive Statistics")
stargazer(visu.data2, summary.stat = c("n", "mean", "sd", "min", "max"), title="Descriptive Statistics (First Generation)")
stargazer(visu.data3, summary.stat = c("n", "mean", "sd", "min", "max"), title="Descriptive Statistics (Second Generation)")



###Simple OLS
data2 <- data
data2$gen <- ifelse(data$first.gen, 1, 
                    ifelse(data$second.gen, 2, 3))
tapply(data2$annual.posttax, data2$gen, mean) #헐

a <- t.test(data2$annual.log[data2$gen == 1], data2$annual.log[data2$gen == 2])  

linear.1 <- lm(data$annual.log ~ data$sex + data$age + data$agesquare)
linear.2 <- lm(data$annual.log ~ data$sex + data$age + data$agesquare + data$eduyear + data$regular + data$serviceyear + data$first.gen)
linear.3 <- lm(data$annual.log ~ data$sex + data$age + data$agesquare + data$eduyear + data$regular + data$serviceyear + data$workyear + data$worksquare + data$size + data$first.gen)
linear.4 <- lm(data$annual.log ~ data$sex + data$age + data$agesquare + data$eduyear + data$regular + data$serviceyear + data$workyear + data$worksquare + data$size + data$type2 + data$year + data$first.gen)

summary(linear.4)

stargazer(linear.2, linear.3, linear.4, 
          covariate.labels = c("SEX", "AGE", "AGE2", "EDU", "REGULAR", "SERVEYEAR", "WORKEXP", "WORKEXP2", "SIZE2", "SIZE3", "WORKTYPE2", "WORKTYPE3", "YEAR", "FIRSTGEN"),
          dep.var.labels   = "Annual Income (Log)")



###Mincer Equation

mincer.1 <- lm(annual.log ~ age + agesquare + eduyear + workyear + worksquare + first.gen, data=data)
summary(mincer.1)
mincer.2 <- lm(annual.log ~ age + agesquare + eduyear + workyear + worksquare + first.gen, data=data[data$sex == 1,])
summary(mincer.2)
mincer.3 <- lm(annual.log ~ age + agesquare + eduyear + workyear + worksquare + first.gen, data=data[data$sex == 0,])
summary(mincer.3)

stargazer(mincer.1, mincer.2, mincer.3, 
          covariate.labels = c("AGE", "AGE2", "EDU", "WORKEXP", "WORKEXP2", "FIRSTGEN"),
          dep.var.labels   = "Annual Income (Log)")



#To get psm variable

linear.1 <- lm(data$annual.log ~ data$sex + data$age + data$agesquare)
linear.2 <- lm(data$annual.log ~ data$sex + data$age + data$agesquare + data$eduyear + data$regular + data$serviceyear)
linear.3 <- lm(data$annual.log ~ data$sex + data$age + data$agesquare + data$eduyear + data$regular + data$serviceyear + data$workyear + data$worksquare + data$size)
linear.4 <- lm(data$annual.log ~ data$sex + data$age + data$agesquare + data$eduyear + data$regular + data$serviceyear + data$workyear + data$worksquare + data$size + data$type2 + data$year)

summary(linear.4)

stargazer(linear.2, linear.3, linear.4, 
          covariate.labels = c("SEX", "AGE", "AGE2", "EDU", "REGULAR", "SERVEYEAR", "WORKEXP", "WORKEXP2", "SIZE2", "SIZE3", "WORKTYPE2", "WORKTYPE3", "YEAR"),
          dep.var.labels   = "Annual Income (Log)")


logit.data <- data
linear.2 <- glm(first.gen ~ age + agesquare + eduyear + status + regular + serviceyear, data = logit.data, family = binomial(link = "logit"))
linear.3 <- glm(first.gen ~ age + agesquare + eduyear + status + regular + serviceyear + workyear + worksquare, data = logit.data, family = binomial(link = "logit"))
linear.4 <- glm(first.gen ~ age + agesquare + eduyear + status + regular + serviceyear + workyear + worksquare + type2, data = logit.data, family = binomial(link = "logit"))

logit.data <- data[data$sex == 1,]
linear.21 <- glm(first.gen ~ age + agesquare + eduyear + status + regular + serviceyear, data = logit.data, family = binomial(link = "logit"))
linear.31 <- glm(first.gen ~ age + agesquare + eduyear + status + regular + serviceyear + workyear + worksquare + size, data = logit.data, family = binomial(link = "logit"))
linear.41 <- glm(first.gen ~ age + agesquare + eduyear + status + regular + serviceyear + workyear + worksquare + size + type2 + year, data = logit.data, family = binomial(link = "logit"))

logit.data <- data[data$sex == 0,]
linear.20 <- glm(first.gen ~ age + agesquare + eduyear + status + regular + serviceyear, data = logit.data, family = binomial(link = "logit"))
linear.30 <- glm(first.gen ~ age + agesquare + eduyear + status + regular + serviceyear + workyear + worksquare + size, data = logit.data, family = binomial(link = "logit"))
linear.40 <- glm(first.gen ~ age + agesquare + eduyear + status + regular + serviceyear + workyear + worksquare + size + type2 + year, data = logit.data, family = binomial(link = "logit"))


summary(linear.)


stargazer(linear.2, linear.3, linear.4,  
          covariate.labels = c("AGE", "AGE2", "EDU", "STATUS", "REGULAR", "SERVEYEAR", "WORKEXP", "WORKEXP2", "SIZE2", "SIZE3", "WORKTYPE2", "WORKTYPE3", "YEAR"),
          dep.var.labels   = "First Generation")

stargazer(linear.21, linear.31, linear.41,  
          covariate.labels = c("AGE", "AGE2", "EDU", "STATUS", "REGULAR", "SERVEYEAR", "WORKEXP", "WORKEXP2", "SIZE2", "SIZE3", "WORKTYPE2", "WORKTYPE3", "YEAR"),
          dep.var.labels   = "First Generation")

stargazer(linear.20, linear.30, linear.40,  
          covariate.labels = c("AGE", "AGE2", "EDU", "STATUS", "REGULAR", "SERVEYEAR", "WORKEXP", "WORKEXP2", "SIZE2", "SIZE3", "WORKTYPE2", "WORKTYPE3", "YEAR"),
          dep.var.labels   = "First Generation")



