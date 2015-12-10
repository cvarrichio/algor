



chars<-as.character(sample(1e3,1e4,TRUE))
system.time(result<-sort(chars))
system.time(result<-sortcpp(chars))


chars<-as.character(sample(1e5,1e6,TRUE))
system.time(result<-sort(chars))
system.time(result<-sortcpp(chars))

ints<-as.integer(sample(1e3,1e4,TRUE))
system.time(result<-sort(ints))
system.time(result<-sortcpp(ints))

ints<-as.integer(sample(1e5,1e6,TRUE))
system.time(result<-sort(ints))
system.time(result<-sortcpp(ints))

nums<-runif(1e4)
system.time(result<-sort(nums))
system.time(result<-sortcpp(nums))

nums<-runif(1e6)
system.time(result<-sort(nums))
system.time(result<-sortcpp(nums))


logs<-as.logical(sample(0:1,1e6,TRUE))
system.time(result<-sort(logs))
system.time(result<-sortcpp(logs))


logs<-as.logical(sample(0:1,1e7,TRUE))
system.time(result<-sort(logs))
system.time(result<-sortcpp(logs))
