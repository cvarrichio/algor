#' Faster implementations of base R functions, including sort, order, and match.
#' 
#' Faster implementations of base R functions, including sort, order, and match.
#' 
#' @name algor
#' @docType package
#' @useDynLib algor
NULL

#' Sorting cectors 
#' 
#' Simplified implementation of sort that is much faster than 
#' @example
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


#' @example
#' chars<-as.character(sample(1e3,1e4,TRUE))
#' system.time(a<-order(chars))
#' system.time(b<-corder(chars))
#' identical(chars[a],chars[b])
#' 
#' ints<-as.integer(sample(1e3,1e4,TRUE))
#' system.time(a<-order(ints))
#' system.time(b<-corder(ints))
#' identical(ints[a],ints[b])
#' 
#' nums<-runif(1e4)
#' system.time(a<-order(nums))
#' system.time(b<-corder(nums))
#' identical(nums[a],nums[b])
#' 
#' logs<-as.logical(sample(0:1,1e6,TRUE))
#' system.time(a<-order(logs))
#' system.time(b<-corder(logs))
#' identical(logs[a],logs[b])
#' 
#' \dontrun
#' chars<-as.character(sample(1e5,1e6,TRUE))
#' system.time(a<-order(chars))
#' system.time(b<-corder(chars)) 
#' 
#' ints<-as.integer(sample(1e5,1e6,TRUE))
#' system.time(result<-order(ints))
#' system.time(result<-corder(ints))
#' 
#' nums<-runif(1e6)
#' system.time(result<-order(nums))
#' system.time(result<-corder(nums)) 
#' 
#' logs<-as.logical(sample(0:1,1e7,TRUE))
#' system.time(result<-order(logs))
#' system.time(result<-corder(logs))
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
#'one<-round(runif(1e3),3)
#'two<-round(runif(1e3),3)
#'system.time(a<-lapply(one, function (x) which(two %in% x)))
#'system.time(b<-cmatches(one,two,type='full',list=TRUE))
#'one<-as.character(1:10000)
#'two<-as.character(sample(1:10000,1e3,TRUE))
#'system.time(a<-lapply(one, function (x) which(two %in% x)))
#'system.time(b<-cmatches(one,two,type='full',list=TRUE))
#'system.time(c<-dplyr::full_join(data.frame(key=one),data.frame(key=two)))
#'one<-as.character(1:100)
#'two<-as.character(sample(1:100,1e2,TRUE))
#'system.time(a<-lapply(one, function (x) which(two %in% x)))
#'system.time(b<-cmatches(one,two,type='full',list=FALSE))
#'system.time(d<-dplyr::full_join(data.frame(key=one),data.frame(key=two)))
#'\dontrun
#'one<-as.integer(1:1000000)
#'two<-as.integer(sample(1:1000000,1e5,TRUE))
#'system.time(a<-lapply(one, function (x) which(two %in% x)))
#'system.time(b<-cmatches(one,two,type='full',list=FALSE,indexes=FALSE))
#'system.time(c<-dplyr::full_join(data.frame(key=one),data.frame(key=two)))
#'system.time({require(data.table); d<-merge(data.table(data.frame(key=one)),data.table(data.frame(key=two)),by='key',all=TRUE,allow.cartesian=TRUE)})
#'one<-as.character(1:1000000)
#'two<-as.character(sample(1:1000000,1e5,TRUE))
#'system.time(a<-lapply(one, function (x) which(two %in% x)))
#'system.time(b<-cmatches(one,two,type='full',list=FALSE,indexes=FALSE))
#'system.time(c<-dplyr::full_join(data.frame(key=one),data.frame(key=two)))
#'system.time({require(data.table); d<-merge(data.table(data.frame(key=one)),data.table(data.frame(key=two)),by='key',all=TRUE,allow.cartesian=TRUE)})

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


# #'@example
# #'df<-data.frame(one=sample(1e4,1e6,TRUE),two=1:1000000)
# #'system.time(a<-split(df,df$one))
#'system.time(a<-split(as.matrix(df),df$one))
#'system.time(b<-lapply(a,function (x) matrix(x,ncol=2)))
# csplit<-function(x,f)
# {
#   
# }
