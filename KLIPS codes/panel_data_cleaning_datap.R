# Mater's thesis
# A Study on Labor Experience of First Generation College Students using Korean Panel Data 
# : The Challenges and Privileges
# Modified 191207 by JISU (ver.2)
# Modified 191215 by JISU (ver.3)

cont.var <- function(datap, year){
  year <- sprintf("%02d", year)
  cols <- NA
  
  cols <- append(cols, which(names(datap) == "pid"))
  #sex
  sex.var <- paste0("p",year,"0101") 
  cols <- append(cols, which(names(datap) == sex.var))
  
  #age 
  age.var <- paste0("p",year,"0107") 
  cols <- append(cols, which(names(datap) == age.var))
  
  #birthyear
  birth.var <- paste0("p",year,"0104") 
  cols <- append(cols, which(names(datap) == birth.var))

  #edu
  edu.var <- paste0("p",year,"0110") 
  cols <- append(cols, which(names(datap) == edu.var))
  #(7) 4 year
  #(8) master
  #(9) phd
  
  #status
  status.var <- paste0("p",year,"0111") 
  cols <- append(cols, which(names(datap) == status.var))
  #(1) graduate
  #(2) completion
  #(3) dropped out
  #(4) in school
  #(5) taking off
  
  #Years of education ends
  eduyear.var <- paste0("p",year,"0114") 
  cols <- append(cols, which(names(datap) == eduyear.var))
  # the period the education ends
  #Mincer equation: number of years of education has to be calculated
  
  # 
  wagem.var <- paste0("p",year,"1642") # monthly wage of salary worker
  cols <- append(cols, which(names(datap) == wagem.var))
  
  wagey.var <- paste0("p",year,"1672") # monthly wage of self-employed worker
  cols <- append(cols, which(names(datap) == wagey.var))
  
  wage.bt.var <- paste0("p",year,"1702") # total pre-tax annual income 
  cols <- append(cols, which(names(datap) == wage.bt.var))
  
  wage.at.var <- paste0("p",year,"1703") # total post-tax annual income
  cols <- append(cols, which(names(datap) == wage.at.var))
 
  # the time work started
  # year
  worky.var <- paste0("p",year,"0301")
  cols <- append(cols, which(names(datap) == worky.var))
  
  # month
  workm.var <- paste0("p",year,"0302")
  cols <- append(cols, which(names(datap) == workm.var))
  
  # date
  workd.var <- paste0("p",year,"0303")
  cols <- append(cols, which(names(datap) == workd.var))

  # regular worker (1: regular 2:temporary)
  reg.var <- paste0("p",year,"0317")
  cols <- append(cols, which(names(datap) == reg.var))
  
  # type of industry
  workt.var <- paste0("p",year,"0332")
  cols <- append(cols, which(names(datap) == workt.var))
  #p__0330 #($)CAV5H@O@Z8.)>wA>: 2000DZ5e(G%AX;j>w:P7y 8Bw)	
  #p__0331 #($)CAV5H@O@Z8.)>wA>: 2007DZ5e(G%AX;j>w:P7y 9Bw)	
  #p__0332 #($)CAV5H@O@Z8.)AwA>: 2000DZ5e(G%AXAw>w:P7y 5Bw)	
  #p__0333 #($)CAV5H@O@Z8.)AwA>: 2007DZ5e(G%AXAw>w:P7y 6Bw)	
  
  # Size of company
  works.var <- paste0("p",year,"0403")
  cols <- append(cols, which(names(datap) == works.var))
  
  cols <- cols[-1]
  return(cols)
}

data <- rep(NA, 20)

#1~5 theres no some variables
for (year in 6:20){
  datap <- read.spss(paste0("/Users/jisulee/Documents/Master's thesis/KLIPS/klips",sprintf("%02d", year),"p.sav"), to.data.frame=TRUE)
  cols <- cont.var(datap, year)

# id sex age edu status "Years of education ends" wage
  yeardata <- datap[,cols]
  yeardata$year <- year
  yeardata$first.gen <- yeardata$pid %in% first.gen[,1][first.gen[,2] == year]
  yeardata$second.gen <- yeardata$pid %in% second.gen[,1][second.gen[,2] == year]
  print(dim(yeardata))
  colnames(yeardata) <- c("pid", "sex", "age", "birthyear", "edu", "status", "eduyear", "msalary", 
                          "mself", "annual.pretax", "annual.posttax", "worky", "workm", "workd", 
                          "regular", "type", "size", "year", "first.gen", "second.gen")
  yeardata <- yeardata[(yeardata$first.gen | yeardata$second.gen),]
  data <- rbind(data, yeardata)
}

data <- data[-1,]

table(data$first.gen)
#FALSE  TRUE 
#879  1872 

data <- data[(!is.na(data$annual.posttax)) | (!is.na(data$msalary)),] 
#table(data$first.gen)
#FALSE  TRUE 
#392  1062 

data[data[,] == -1] = NA # No reply

data$sex <- data$sex - 1 # male 0 female 1
data$sex <- data$sex == 0 # male 1 female 0
data$regular <- data$regular - 1
data$regular <- data$regular == 0 #regular 1 nonregular 0
data$agesquare <- (data$age)^2
data$birthyear <- data$birthyear + 7 

data$edu[(data$status == 3) & !is.na(data$status)] <- data$edu[(data$status == 3) & !is.na(data$status)] - 1
data$eduyear <- ifelse((data$edu == 1) | data$edu == 2, 0, 
                       ifelse(data$edu == 3, 6, 
                              ifelse(data$edu == 4, 9, 
                                     ifelse(data$edu == 5, 12, 
                                            ifelse(data$edu == 6, 14, 
                                                   ifelse(data$edu == 7, 16, 
                                                          ifelse(data$edu == 8, 18, 22
                                                                 )))))))

data$annual.posttax[(!is.na(data$msalary) & is.na(data$annual.posttax))] <- data$msalary[(!is.na(data$msalary) & is.na(data$annual.posttax))] * 12

data$annual.log <- log(data$annual.posttax)
data$annual.log[which(is.nan(data$annual.log))] = NA
data$annual.log[which(data$annual.log==Inf)] = NA
data$annual.log[which(data$annual.log==-Inf)] = NA
data$serviceyear <- 1997 + year - data$worky #####
data$workyear <- data$age - data$edu - 6

data$type2 <- ifelse(substr(data$type,1,1) < 4, 1, 
         ifelse((4 <= substr(data$type,1,1)) & (substr(data$type,1,1) < 6), 2, 3))
data$type2 <- as.factor(data$type2)


data$size <- ifelse(data$size == 11, NA,
                    ifelse(data$size < 4, 1, 
                      ifelse(data$size > 7, 3, 2)))
data$size <- as.factor(data$size)

#(1) 1-4
#(2) 5-9
#(3) 10-29
#(4) 30-49
#(5) 50-69
#(6) 70-99
#(7) 100-299
#(8) 300-499
#(9) 500-999
#(10) more than 1000
#(11) no idea


data$worksquare <- (data$workyear)^2

#1,2,3 professional and clerical positions
#4,5 service and Sales Jobs
#6,7,8,9 agriculture, fishing, machinery, simple labor

#data3 <- data2[data2$gen !=3, ]

#data3$gen <- as.factor(data3$gen)
#fit <- lm(data3$annual.log ~ data3$sex + data3$age + data3$agesquare + data3$eduyear + data3$regular + data3$serviceyear + data3$workyear + data3$type2 + data3$gen)
#summary(fit)

#table(data$first.gen[(data$edu >=6)], data$second.gen[(data$edu >=6)])
#table(data$first.gen, data$second.gen)

#
#FALSE TRUE
#FALSE  8059  118
#TRUE    298    0





