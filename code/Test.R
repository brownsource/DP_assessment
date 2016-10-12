#A <- c(1,1,0,1)
#B <- c(3,3,"n/a",3)
#DF <- data.frame(A,B)
#C <- DF$A * DF$B

foo <- data.frame("day"= c(1, 3, 5, 7), "od" = c(0.1, "N/A", 0.4, 0.8)) 
NAs <- foo == "N/A"
foo[is.na(NAs)] <- 0
