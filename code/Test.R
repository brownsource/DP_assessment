A <- c(1,1,"N/A",0)
B <- c(0,1,"n/a","n/a")
C <- c(3,3,"n/a","n/a")


DF <- data.frame(A,B,C,D)

D <- ifelse (A == "N/A", "",
            ifelse (A == 0, 0,
                   ((A * B) * C) / 4
                   )
            )
