#' Faster implementations of base R functions, including sort, order, and match.
#' 
#' Faster implementations of base R functions, including sort, order, and match.
#' 
#' @name algor
#' @docType package
#' @useDynLib algor
NULL


#' chars<-as.character(sample(1e3,1e4,TRUE))
#' system.time(a<-sort(chars))
#' system.time(b<-csort(chars))
#  
# chars<-as.character(sample(1e5,1e6,TRUE))
# system.time(a<-sort(chars))
# system.time(b<-csort(chars))
# identical(a,b)

# ints<-as.integer(sample(1e3,1e4,TRUE))
# system.time(result<-sort(ints))
# system.time(result<-csort(ints))
# 
# ints<-as.integer(sample(1e5,1e6,TRUE))
# system.time(result<-sort(ints))
# system.time(result<-csort(ints))
# 
# nums<-runif(1e4)
# system.time(result<-sort(nums))
# system.time(result<-csort(nums))
# 
# nums<-runif(1e6)
# system.time(result<-sort(nums))
# system.time(result<-csort(nums))
# 
# 
# logs<-as.logical(sample(0:1,1e6,TRUE))
# system.time(result<-sort(logs))
# system.time(result<-csort(logs))
# 
# 
# logs<-as.logical(sample(0:1,1e7,TRUE))
# system.time(result<-sort(logs))
# system.time(result<-csort(logs))
csort<-function(x)
{
  result<-.Call('csort',x)
  return(result)
}


#' chars<-as.character(sample(1e3,1e4,TRUE))
#' system.time(a<-order(chars))
#' system.time(b<-corder(chars))
#  
# chars<-as.character(sample(1e5,1e6,TRUE))
# system.time(a<-order(chars))
# system.time(b<-corder(chars))
# identical(chars[a],chars[b])

# ints<-as.integer(sample(1e3,1e4,TRUE))
# system.time(result<-order(ints))
# system.time(result<-corder(ints))
# 
# ints<-as.integer(sample(1e5,1e6,TRUE))
# system.time(result<-order(ints))
# system.time(result<-corder(ints))
# 
# nums<-runif(1e4)
# system.time(result<-order(nums))
# system.time(result<-corder(nums))
# 
# nums<-runif(1e6)
# system.time(result<-order(nums))
# system.time(result<-corder(nums))
# 
# 
# logs<-as.logical(sample(0:1,1e6,TRUE))
# system.time(result<-order(logs))
# system.time(result<-corder(logs))
# 
# 
# logs<-as.logical(sample(0:1,1e7,TRUE))
# system.time(result<-order(logs))
# system.time(result<-corder(logs))
corder<-function(x)
{
  result<-.Call('ordercpp',x)
  return(result)
}
#'@examples
#'one<-as.integer(1:10000)
#'two<-as.integer(sample(1:10000,1e3,TRUE))
#'system.time(a<-lapply(one, function (x) which(two %in% x)))
#'system.time(b<-cmatches(one,two,type='full',list=TRUE))
#'\dontrun
#'one<-as.integer(1:1000000)
#'two<-as.integer(sample(1:1000000,1e5,TRUE))
#'system.time(a<-lapply(one, function (x) which(two %in% x)))
#'system.time(b<-cmatches(one,two,type='full',list=FALSE))
#'system.time(d<-dplyr::full_join(data.frame(key=one),data.frame(key=two)))
#'system.time({require(data.table); d<-merge(data.table(data.frame(key=one)),data.table(data.frame(key=two)),by='key',all=TRUE,allow.cartesian=TRUE)})
#'
#'


cmatches<-function(a,b,type='left',list=FALSE,indexes=TRUE)
{
  result<-.Call('matches',a,b)
  result<-data.frame(a=result[[1]],b=result[[2]])
  if(type=='left')
    result<-result[!is.na(result$a),]
  if(!indexes)
  {
    result$a<-a[result$a]
    result$b<-b[result$b]
  }
  if(list)
    result<-tapply(result$b,result$a,identity)
  return(result)
}
