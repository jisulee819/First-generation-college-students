




count.target <- function(datap, datah, year, fat.code, mot.code, self.code){
  
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
  
  
  
  num.target <- sum(((!is.na(fathers2$edu)) | (!is.na(mothers2$edu))) & ((selfs2$edu >= 7)))
  print(paste0(fat.code, mot.code, self.code))
 
  return(num.target)  
}




count.target2 <- function(datap, datah, year){
  
  first.gen <- NA
  
  first.gen <- append(first.gen, count.target(datap, datah, year, 5,6,10))
  first.gen <- append(first.gen, count.target(datap, datah, year, 7,8,20))
  first.gen <- append(first.gen, count.target(datap, datah, year, 10,20,11))
  first.gen <- append(first.gen, count.target(datap, datah, year, 10,20,12))
  first.gen <- append(first.gen, count.target(datap, datah, year, 10,20,13))
  first.gen <- append(first.gen, count.target(datap, datah, year, 10,20,14))
  first.gen <- append(first.gen, count.target(datap, datah, year, 10,20,15))
  first.gen <- append(first.gen, count.target(datap, datah, year, 10,20,16))
  first.gen <- append(first.gen, count.target(datap, datah, year, 10,20,17))
  first.gen <- append(first.gen, count.target(datap, datah, year, 11,21,111))
  first.gen <- append(first.gen, count.target(datap, datah, year, 11,21,112))
  first.gen <- append(first.gen, count.target(datap, datah, year, 11,21,113))
  first.gen <- append(first.gen, count.target(datap, datah, year, 11,21,114))
  first.gen <- append(first.gen, count.target(datap, datah, year, 11,21,115))
  first.gen <- append(first.gen, count.target(datap, datah, year, 12,22,121))
  first.gen <- append(first.gen, count.target(datap, datah, year, 12,22,122))
  first.gen <- append(first.gen, count.target(datap, datah, year, 12,22,123))
  first.gen <- append(first.gen, count.target(datap, datah, year, 13,23,131))
  first.gen <- append(first.gen, count.target(datap, datah, year, 13,23,132))
  first.gen <- append(first.gen, count.target(datap, datah, year, 14,24,141))
  first.gen <- append(first.gen, count.target(datap, datah, year, 14,24,142))
  first.gen <- append(first.gen, count.target(datap, datah, year, 14,24,143))
  first.gen <- append(first.gen, count.target(datap, datah, year, 15,25,151))
  first.gen <- append(first.gen, count.target(datap, datah, year, 15,25,152))
  first.gen <- append(first.gen, count.target(datap, datah, year, 16,26,161))
  first.gen <- append(first.gen, count.target(datap, datah, year, 31,51,311))
  first.gen <- append(first.gen, count.target(datap, datah, year, 31,51,312))
  first.gen <- append(first.gen, count.target(datap, datah, year, 31,51,313))
  first.gen <- append(first.gen, count.target(datap, datah, year, 111,211,1111))
  first.gen <- append(first.gen, count.target(datap, datah, year, 111,211,1112))
  
  first.gen <- first.gen[-1]
  
  return(first.gen)
}


a <- count.target2(d20p, d20h, 20)
sum(a)


