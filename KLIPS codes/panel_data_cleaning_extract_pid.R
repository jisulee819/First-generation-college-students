
# Mater's thesis
# A Study on Labor Experience of First Generation College Students using Korean Panel Data 
# : The Challenges and Privileges
# Modified 191207 by JISU (ver.2)
# Modified 191215 by JISU (ver.3)
# : Automized every process of data cleaning :)

library(prob)
library(foreign)

if.1st.gen <- function(fat.edu, fat.status, mot.edu, mot.status, self.edu, self.status){
  #Criteria: "Attend" to college
  x <- (fat.edu < 7) & (mot.edu < 7) & (self.edu >=7)
  # even though "self" has dropped the college out, she is counted as first generation college student
  return(x)
}

return.colnum <- function(agent, datah, year){
  if (is.vector(agent)){
    agent.var <- paste0("h", year, "0", agent[2]+220)
    call.agent <- which(names(datah) == agent.var)
  } else {
    agent.var <- paste0("h", year, "0", agent[,2]+220)
    call.agent <- NA
    for (i in c(1:length(agent.var))){
      call.agent <- append(call.agent, which(names(datah) == agent.var[i]))
    }
    call.agent <- call.agent[-1]
  }
  
  return(call.agent)
}

print.target <- function(datap, datah, year, fat.code, mot.code, self.code){
  
  year <- sprintf("%02d", year)
  reply.var <- paste0("hwave",year)
  call.reply <- which(names(datah) == reply.var)
  datah <- datah[datah[,call.reply]==1,]
  
  relat.var <- paste0("h",year,"0261")
  call.relat <- which(names(datah) == relat.var)
  
  selfs <- which(datah[,call.relat:(call.relat+14)] == self.code, arr.ind = TRUE)
  fathers <- which(datah[,call.relat:(call.relat+14)] == fat.code, arr.ind = TRUE)
  mothers <- which(datah[,call.relat:(call.relat+14)] == mot.code, arr.ind = TRUE)
  
  selfs[,2] <- return.colnum(selfs, datah, year)
  fathers[,2] <- return.colnum(fathers, datah, year)
  mothers[,2] <- return.colnum(mothers, datah, year)
  
  target <- intersect(intersect(selfs[,1], fathers[,1]), mothers[,1]) 
  #the cases both father and mother exists in the datapool
  
  if (is.vector(selfs)){} else {
    selfs <- selfs[selfs[,1] %in% target,]}
  if (is.vector(fathers)){} else {
    fathers <- fathers[fathers[,1] %in% target,]}
  if (is.vector(mothers)){} else {
    mothers <- mothers[mothers[,1] %in% target,]}
  
  if (is.vector(selfs)){
    self.id <- datah[selfs[1], selfs[2]]
  } else {
    selfs <- selfs[order(selfs[,1]),]
    self.id <- datah[selfs]}
  if (is.vector(fathers)){
    fat.id <- datah[fathers[1], fathers[2]]
  } else {
    fathers <- fathers[order(fathers[,1]),]
    fat.id <- datah[fathers] 
    }
  if (is.vector(mothers)){
    mot.id <- datah[mothers[1], mothers[2]]
  } else {
    mothers <- mothers[order(mothers[,1]),]
    mot.id <- datah[mothers]
    }
  
  # When the householder who was originally sample of 98 investigation 
  # is reported as a new family member in sample of 09 : 
  # self ID follows original family code
  # his parents follows new family code
  
  edu.var <- paste0("p",year,"0110")
  stat.var <- paste0("p",year,"0111")
  
  call.edu <- which(names(datap) == edu.var)
  call.status <- which(names(datap) == stat.var)
  
  fat.h.id <- datap[datap$pid %in% fat.id, 2]
  fat.edu <- datap[datap$pid %in% fat.id, call.edu]
  fat.status <- datap[datap$pid %in% fat.id, call.status]
  mot.h.id <- datap[datap$pid %in% mot.id, 2]
  mot.edu <- datap[datap$pid %in% mot.id, call.edu]
  mot.status <- datap[datap$pid %in% mot.id, call.status]
  self.h.id <- datap[datap$pid %in% self.id, 2]
  self.s.id <- datap[datap$pid %in% self.id, 1]
  self.edu <- datap[datap$pid %in% self.id, call.edu]
  self.status <- datap[datap$pid %in% self.id, call.status]
  
  fathers2 <- data.frame(id=fat.h.id, edu=fat.edu, status=fat.status)
  mothers2 <- data.frame(id=mot.h.id, edu=mot.edu, status=mot.status)
  selfs2 <- data.frame(id=self.h.id, edu=self.edu, status=self.status, pid=self.s.id)
  target2 <- intersect(intersect(selfs2[,1], fathers2[,1]), mothers2[,1])
  
  selfs2 <- selfs2[selfs2[,1] %in% target2,]
  fathers2 <- fathers2[fathers2[,1] %in% target2,]
  mothers2 <- mothers2[mothers2[,1] %in% target2,]
  selfs2 <- selfs2[order(selfs2[,1]),]
  fathers2 <- fathers2[order(fathers2[,1]),]
  mothers2 <- mothers2[order(mothers2[,1]),]

  if.target <- if.1st.gen(fathers2$edu, fathers2$status, mothers2$edu, mothers2$status, selfs2$edu, selfs2$status)
  target.id <- selfs2$pid[if.target]
  
  print(paste0(fat.code, mot.code, self.code))
  return(target.id)  
}

return.target <- function(datap, datah, year){
  
  first.gen <- NA
  
  first.gen <- append(first.gen, print.target(datap, datah, year, 5,6,10))
  first.gen <- append(first.gen, print.target(datap, datah, year, 7,8,20))
  first.gen <- append(first.gen, print.target(datap, datah, year, 10,20,11))
  first.gen <- append(first.gen, print.target(datap, datah, year, 10,20,12))
  first.gen <- append(first.gen, print.target(datap, datah, year, 10,20,13))
  first.gen <- append(first.gen, print.target(datap, datah, year, 10,20,14))
  first.gen <- append(first.gen, print.target(datap, datah, year, 10,20,15))
  first.gen <- append(first.gen, print.target(datap, datah, year, 10,20,16))
  first.gen <- append(first.gen, print.target(datap, datah, year, 10,20,17))
  first.gen <- append(first.gen, print.target(datap, datah, year, 11,21,111))
  first.gen <- append(first.gen, print.target(datap, datah, year, 11,21,112))
  first.gen <- append(first.gen, print.target(datap, datah, year, 11,21,113))
  first.gen <- append(first.gen, print.target(datap, datah, year, 11,21,114))
  first.gen <- append(first.gen, print.target(datap, datah, year, 11,21,115))
  first.gen <- append(first.gen, print.target(datap, datah, year, 12,22,121))
  first.gen <- append(first.gen, print.target(datap, datah, year, 12,22,122))
  first.gen <- append(first.gen, print.target(datap, datah, year, 12,22,123))
  first.gen <- append(first.gen, print.target(datap, datah, year, 13,23,131))
  first.gen <- append(first.gen, print.target(datap, datah, year, 13,23,132))
  first.gen <- append(first.gen, print.target(datap, datah, year, 14,24,141))
  first.gen <- append(first.gen, print.target(datap, datah, year, 14,24,142))
  first.gen <- append(first.gen, print.target(datap, datah, year, 14,24,143))
  first.gen <- append(first.gen, print.target(datap, datah, year, 15,25,151))
  first.gen <- append(first.gen, print.target(datap, datah, year, 15,25,152))
  first.gen <- append(first.gen, print.target(datap, datah, year, 16,26,161))
  first.gen <- append(first.gen, print.target(datap, datah, year, 31,51,311))
  first.gen <- append(first.gen, print.target(datap, datah, year, 31,51,312))
  first.gen <- append(first.gen, print.target(datap, datah, year, 31,51,313))
  first.gen <- append(first.gen, print.target(datap, datah, year, 111,211,1111))
  first.gen <- append(first.gen, print.target(datap, datah, year, 111,211,1112))
  
  first.gen <- first.gen[-1]
  
  return(first.gen)
}

#d20p <- read.spss("/Users/jisulee/Documents/Master's thesis/KLIPS/klips20p.sav", to.data.frame=TRUE)
#d20h <- read.spss("/Users/jisulee/Documents/Master's thesis/KLIPS/klips20h.sav", to.data.frame=TRUE)
#d19p <- read.spss("/Users/jisulee/Documents/Master's thesis/KLIPS/klips19p.sav", to.data.frame=TRUE)
#d19h <- read.spss("/Users/jisulee/Documents/Master's thesis/KLIPS/klips19h.sav", to.data.frame=TRUE)

#first.gen.20 <- return.target(d20p, d20h, 20)

#first.gen.19 <- return.target(d19p, d19h, 19) 


for (year in 1:20){
  varname <- paste0("first.gen.",year)
  datap <- read.spss(paste0("/Users/jisulee/Documents/Master's thesis/KLIPS/klips",sprintf("%02d", year),"p.sav"), to.data.frame=TRUE)
  datah <- read.spss(paste0("/Users/jisulee/Documents/Master's thesis/KLIPS/klips",sprintf("%02d", year),"h.sav"), to.data.frame=TRUE)
  
  a <- return.target(datap, datah, year)
  assign(varname, a)
}


first.gen <- c(first.gen.1, first.gen.2, first.gen.3, first.gen.4, first.gen.5, 
               first.gen.6, first.gen.7, first.gen.8, first.gen.9, first.gen.10, 
               first.gen.11, first.gen.12, first.gen.13, first.gen.14, first.gen.15, 
               first.gen.16, first.gen.17, first.gen.18, first.gen.19, first.gen.20)
first.gen <- unique(first.gen)
first.gen <- data.frame(first.gen, NA)


for (year in 1:20){
  varname <- paste0("first.gen.",year)
  assigned <- (first.gen[,1] %in% get(varname))
  first.gen[,2][assigned] <- year
  
  #1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
  #106  96  94  88  82  91  85  75  94 129  86 128  87  77  74 105  72  88  97 584 
  # To Assign the pids for the latest datasets
}



