
# Mater's thesis
# A Study on Labor Experience of First Generation College Students using Korean Panel Data 
# : The Challenges and Privileges
# Created 191215 by JISU (ver.1)
# To solve parents duplicating problem

if.2nd.gen <- function(par.edu, par.status, self.edu, self.status){
  #Criteria: "Attend" to college
  x <- (par.edu >= 7) & (self.edu >=7)
  return(x)
}

print.2nd <- function(datap, datah, year, par.code, self.code){
  
  year <- sprintf("%02d", year)
  reply.var <- paste0("hwave",year)
  call.reply <- which(names(datah) == reply.var)
  datah <- datah[datah[,call.reply]==1,]
  
  relat.var <- paste0("h",year,"0261")
  call.relat <- which(names(datah) == relat.var)
  
  selfs <- which(datah[,call.relat:(call.relat+14)] == self.code, arr.ind = TRUE)
  parents <- which(datah[,call.relat:(call.relat+14)] == par.code, arr.ind = TRUE)

  selfs[,2] <- return.colnum(selfs, datah, year)
  parents[,2] <- return.colnum(parents, datah, year)

  target <- intersect(selfs[,1], parents[,1])  ####mom or par may not exist
  #the cases both parher and mother exists in the datapool
  
  if (is.vector(selfs)){} else {
    selfs <- selfs[selfs[,1] %in% target,]}
  if (is.vector(parents)){} else {
    parents <- parents[parents[,1] %in% target,]}

  if (is.vector(selfs)){
    self.id <- datah[selfs[1], selfs[2]]
  } else {
    selfs <- selfs[order(selfs[,1]),]
    self.id <- datah[selfs]}
  if (is.vector(parents)){
    par.id <- datah[parents[1], parents[2]]
  } else {
    parents <- parents[order(parents[,1]),]
    par.id <- datah[parents] 
  }
  edu.var <- paste0("p",year,"0110")
  stat.var <- paste0("p",year,"0111")
  
  call.edu <- which(names(datap) == edu.var)
  call.status <- which(names(datap) == stat.var)
  
  par.h.id <- datap[datap$pid %in% par.id, 2]
  par.edu <- datap[datap$pid %in% par.id, call.edu]
  par.status <- datap[datap$pid %in% par.id, call.status]
  self.h.id <- datap[datap$pid %in% self.id, 2]
  self.s.id <- datap[datap$pid %in% self.id, 1]
  self.edu <- datap[datap$pid %in% self.id, call.edu]
  self.status <- datap[datap$pid %in% self.id, call.status]
  
  parents2 <- data.frame(id=par.h.id, edu=par.edu, status=par.status)
  selfs2 <- data.frame(id=self.h.id, edu=self.edu, status=self.status, pid=self.s.id)
  target2 <- intersect(selfs2[,1], parents2[,1])
  
  selfs2 <- selfs2[selfs2[,1] %in% target2,]
  parents2 <- parents2[parents2[,1] %in% target2,]
  selfs2 <- selfs2[order(selfs2[,1]),]
  parents2 <- parents2[order(parents2[,1]),]

  if.target <- if.2nd.gen(parents2$edu, parents2$status, selfs2$edu, selfs2$status)
  target.id <- selfs2$pid[if.target]
  
  print(paste0(par.code, self.code))
  return(target.id)  
}

return.2nd <- function(datap, datah, year){
  
  second.gen <- NA
  
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 5,10))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 6,10))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 7,20))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 8,20))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 10,11))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 20,11))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 10,12))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 10,13))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 10,14))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 10,15))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 10,16))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 10,17))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 20,12))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 20,13))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 20,14))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 20,15))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 20,16))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 20,17))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 11,111))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 11,112))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 11,113))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 11,114))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 11,115))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 12,121))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 12,122))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 12,123))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 13,131))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 13,132))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 14,141))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 14,142))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 14,143))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 15,151))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 15,152))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 16,161))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 31,311))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 31,312))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 31,313))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 21,111))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 21,112))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 21,113))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 21,114))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 21,115))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 22,121))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 22,122))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 22,123))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 23,131))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 23,132))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 24,141))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 24,142))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 24,143))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 25,151))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 25,152))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 26,161))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 51,311))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 51,312))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 51,313))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 111,1111))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 111,1112))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 211,1111))
  second.gen <- append(second.gen, print.2nd(datap, datah, year, 211,1112))
  
  second.gen <- second.gen[-1]
  
  return(second.gen)
}

#second.gen.20 <- return.2nd(d20p, d20h, 20)
#d20p <- read.spss("/Users/jisulee/Documents/Master's thesis/KLIPS/klips20p.sav", to.data.frame=TRUE)
#d20h <- read.spss("/Users/jisulee/Documents/Master's thesis/KLIPS/klips20h.sav", to.data.frame=TRUE)

#second.gen.20 <- unique(second.gen.20)



for (year in 01:20){
  varname <- paste0("second.gen.", year)
  datap <- read.spss(paste0("/Users/jisulee/Documents/Master's thesis/KLIPS/klips",sprintf("%02d", year),"p.sav"), to.data.frame=TRUE)
  datah <- read.spss(paste0("/Users/jisulee/Documents/Master's thesis/KLIPS/klips",sprintf("%02d", year),"h.sav"), to.data.frame=TRUE)
  
  a <- return.2nd(datap, datah, year)
  assign(varname, a)
}


second.gen <- c(second.gen.1, second.gen.2, second.gen.3, second.gen.4, second.gen.5, 
                second.gen.6, second.gen.7, second.gen.8, second.gen.9, second.gen.10, 
                second.gen.11, second.gen.12, second.gen.13, second.gen.14, second.gen.15, 
                second.gen.16, second.gen.17, second.gen.18, second.gen.19, second.gen.20)
second.gen <- unique(second.gen)
second.gen <- data.frame(second.gen, NA)


for (year in 1:20){
  varname <- paste0("second.gen.",year)
  
  assigned <- (second.gen[,1] %in% get(varname))
  second.gen[,2][assigned] <- year
  
  #table(second.gen[,2])
  #  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
  #56  27  26  28  17  39  48  31  35  40  40  40  37  39  48  29  25  61  49 318 10  11  12  13  14  15  16  17  18  19  20 
  # To Assign the pids for the latest datasets
}


