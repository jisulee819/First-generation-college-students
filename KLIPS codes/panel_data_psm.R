##### Propensity Score Matching: Optimal 1:1 method  
library(MatchIt)
library(optmatch)
library(dplyr)
library(ggplot2)

write.csv(data, "/Users/jisulee/Documents/Master's thesis/KLIPS_cleaned_data.csv")
#Shapiro test

#binary to factor (year)


psm.data <- data.frame(sex = data$sex, age = data$age, agesquare = data$agesquare, eduyear = data$eduyear, status = data$status, 
                       regular = data$regular, serviceyear = data$serviceyear, workyear = data$workyear, worksquare = data$worksquare, 
                       size2 = data$size == 2, size3 = data$size == 3, type2 = data$type2 == 2, type3 = data$type2 == 3, year = data$year, wage = data$annual.log, first.gen = data$first.gen)
# you need to include any covariate that is related to both the treatment assignment and potential outcomes.

psm.data <- data.frame(sex = data$sex, age = data$age, eduyear = data$eduyear,
                       regular = data$regular, serviceyear = data$serviceyear, size2 = data$size == 2, size3 = data$size == 3,
                       type2 = data$type2 == 2, type3 = data$type2 == 3, wage = data$annual.log, year = data$year, first.gen = data$first.gen)


psm.data <- na.omit(psm.data)


opt.psm.out = matchit(!first.gen ~ age + agesquare + eduyear + status +
                        regular + serviceyear + workyear + worksquare + size2 + size3 + 
                        type2 + type3 + year, method = "optimal", data = data1)



#This argument specifies a matching method. Currently, "exact" (exact match- ing), "full" (full matching), 
#"genetic" (genetic matching), "nearest" (nearest neighbor matching), "optimal" (optimal matching), and 
#"subclass" (sub-classification) are available. 

opt.psm.out = matchit(!first.gen ~ age + agesquare + eduyear + status +
                      regular + serviceyear + workyear + worksquare +
                      type2 + type3,  method = "nearest", data = data1)



opt.psm.out = matchit(!first.gen ~ age + eduyear + 
                        regular + serviceyear + size2 + size3 + 
                        type2 + type3 + year, method = "nearest", data = data1)
summary(opt.psm.out)
opt.data = match.data(opt.psm.out)

cov <- data1[,-ncol(data1)]
treated = data1$first.gen == 1
std.diff.b = apply(cov, 2, function(x) 100*(mean(x[treated]) - mean(x[!treated]))/(.5*(var(x[treated])+var(x[!treated]))))
std.diff.b

cov <- opt.data[,-c((ncol(opt.data)-2):ncol(opt.data))]
treated = opt.data$first.gen == 1
std.diff.a = apply(cov, 2, function(x) 100*(mean(x[treated]) - mean(x[!treated]))/(.5*(var(x[treated])+var(x[!treated]))))
std.diff.a



### Check the propensity scores
### Visualization
m_ps <- glm(!first.gen ~ age + eduyear + 
              regular + serviceyear + size2 + size3 + 
              type2 + type3 + year,
            family = binomial(), data = data1)
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     first.gen = !m_ps$model$`!first.gen`)
head(prs_df)

labs <- paste("Type of College Students:", c("First Generation", "Second Generation"))
prs_df %>%
  mutate(first.gen = ifelse(first.gen == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~first.gen) +
  xlab("Probability of being the first generation") +
  theme_bw()



fgen <- opt.data[(opt.data$first.gen == 1) & (opt.data$sex == 1),]
sgen <- opt.data[(opt.data$first.gen == 0) & (opt.data$sex == 1),]

t.test(fgen$wage,sgen$wage)

fgen <- opt.data[(opt.data$first.gen == 1) & (opt.data$sex == 0),]
sgen <- opt.data[(opt.data$first.gen == 0) & (opt.data$sex == 0),]

t.test(fgen$wage,sgen$wage)

fgen <- opt.data[(opt.data$first.gen == 1),]
sgen <- opt.data[(opt.data$first.gen == 0),]

t.test(fgen$wage, sgen$wage)

t.test(opt.data$wage ~ opt.data$first.gen)


#Because I had more control than treatment units, 
#optmatch was unable to assign all control units to at least one treatment, which led to the error above. 
#The solution was to switch the control and treatment populations and use a matching ratio of 1 
#in order to match all control units to a treatment unit.

#Bonus: the optimal matches were an even better fit than the "nearest neighbor" matches, which I guess is to be expected.