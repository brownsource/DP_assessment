groupid<-rep(1:5, each=3)
names<-rep(c("Bill", "Jim", "Sarah", "Mike", "Jennifer"),3)
test1<-rep(c(90, 70, 90, NA, 100),3)
test2<-rep(c(80, NA, 92, 80, 65), 3)
testscores<-data.frame(groupid, names, test1, test2)

testscores

testscores$testMean <- rowMeans(testscores[,3:4], na.rm=TRUE)

testscores


