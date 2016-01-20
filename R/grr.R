#' Alternative Implementations of Base R Functions
#' 
#' Alternative implementations of some base R functions, including sort, order, and match.  Functions are
#' simplified but can be faster or have other advantages.  See the documentation of individual functions
#' for details and benchmarks.
#' 
#' @name grr
#' @docType package
#' @useDynLib grr
NULL

#' Sorting vectors
#' 
#' Simplified implementation of \code{\link{sort}}. For large vectors,
#' typically is about 2x faster for numbers and 20x faster for characters and
#' factors.
#' 
#' @param x a vector of class numeric, integer, character, factor, or logical. 
#'   Long vectors are not supported.
#' @export
#' @examples
#' chars<-as.character(sample(1e3,1e4,TRUE))
#' system.time(a<-sort(chars))
#' system.time(b<-sort2(chars))
#' identical(a,b)  
#'  
#' ints<-as.integer(sample(1e3,1e4,TRUE))
#' system.time(a<-sort(ints))
#' system.time(b<-sort2(ints))
#' identical(a,b)
#'  
#' nums<-runif(1e4)
#' system.time(a<-sort(nums))
#' system.time(b<-sort2(nums))
#' identical(a,b)
#' 
#' logs<-as.logical(sample(0:1,1e6,TRUE))
#' system.time(result<-sort(logs))
#' system.time(result<-sort2(logs))
#' 
#' facts<-as.factor(as.character(sample(1e3,1e4,TRUE)))
#' system.time(a<-sort(facts))
#' system.time(b<-sort2(facts))
#' identical(a,b)
#' 
#' #How are special values like NA and Inf handled?
#' #For numerics, values sort intuitively, with the important note that NA and
#' #NaN will come after all real numbers but before Inf.
#' sort2(c(1,2,NA,NaN,Inf,-Inf))
#' #For characters, values sort correctly with NA at the end.
#' sort2(c('C','B',NA,'A'))
#' #For factors, values sort correctly with NA at the beginning.
#' sort2(as.factor(c('C','B',NA,'A')))
#' 
#' \dontrun{
#' chars<-as.character(sample(1e5,1e6,TRUE))
#' system.time(a<-sort(chars))
#' system.time(b<-sort2(chars))
#' 
#' ints<-as.integer(sample(1e5,1e6,TRUE))
#' system.time(result<-sort(ints))
#' system.time(result<-sort2(ints))
#' 
#' nums<-runif(1e6)
#' system.time(result<-sort(nums))
#' system.time(result<-sort2(nums))
#' 
#' logs<-as.logical(sample(0:1,1e7,TRUE))
#' system.time(result<-sort(logs))
#' system.time(result<-sort2(logs))
#' 
#' facts<-as.factor(as.character(sample(1e5,1e6,TRUE)))
#' system.time(a<-sort(facts))
#' system.time(b<-sort2(facts))
#' }
sort2<-function(x)
{
  result<-.Call('sortcpp',x)
  return(result)
}

#' Ordering vectors
#' 
#' Simplified implementation of \code{\link{order}}.  For large vectors, typically is about 3x faster for 
#' numbers and 20x faster for characters.
#' 
#' @param x a vector of class numeric, integer, character, factor, or logical.  Long vectors are not supported.
#' @export
#' @examples
#' chars<-as.character(sample(1e3,1e4,TRUE))
#' system.time(a<-order(chars))
#' system.time(b<-order2(chars))
#' identical(chars[a],chars[b])
#' 
#' ints<-as.integer(sample(1e3,1e4,TRUE))
#' system.time(a<-order(ints))
#' system.time(b<-order2(ints))
#' identical(ints[a],ints[b])
#' 
#' nums<-runif(1e4)
#' system.time(a<-order(nums))
#' system.time(b<-order2(nums))
#' identical(nums[a],nums[b])
#' 
#' logs<-as.logical(sample(0:1,1e6,TRUE))
#' system.time(a<-order(logs))
#' system.time(b<-order2(logs))
#' identical(logs[a],logs[b])
#' 
#' facts<-as.factor(as.character(sample(1e3,1e4,TRUE)))
#' system.time(a<-order(facts))
#' system.time(b<-order2(facts))
#' identical(facts[a],facts[b])
#' 
#' #How are special values like NA and Inf handled?
#' #For numerics, values sort intuitively, with the important note that NA and
#' #NaN will come after all real numbers but before Inf.
#' (function (x) x[order2(x)])(c(1,2,NA,NaN,Inf,-Inf))
#' #For characters, values sort correctly with NA at the end.
#' (function (x) x[order2(x)])(c('C','B',NA,'A'))
#' #For factors, values sort correctly with NA at the end.
#' (function (x) x[order2(x)])(as.factor(c('C','B',NA,'A')))
#' 
#' \dontrun{
#' chars<-as.character(sample(1e5,1e6,TRUE))
#' system.time(a<-order(chars))
#' system.time(b<-order2(chars)) 
#' 
#' ints<-as.integer(sample(1e5,1e6,TRUE))
#' system.time(result<-order(ints))
#' system.time(result<-order2(ints))
#' 
#' nums<-runif(1e6)
#' system.time(result<-order(nums))
#' system.time(result<-order2(nums)) 
#' 
#' logs<-as.logical(sample(0:1,1e7,TRUE))
#' system.time(result<-order(logs))
#' system.time(result<-order2(logs))
#' 
#' facts<-as.factor(as.character(sample(1e5,1e6,TRUE)))
#' system.time(a<-order(facts))
#' system.time(b<-order2(facts))
#' identical(facts[a],facts[b])
#' 
#' 
#' }
order2<-function(x)
{
  result<-.Call('ordercpp',x)
  return(result)
}


#' Value Matching
#' 
#' Returns a lookup table or list of the positions of ALL matches of its first argument in its second and vice versa.
#' Similar to \code{\link{match}}, though that function only returns the first match.
#' 
#' This behavior can be imitated by using joins to create lookup tables, but \code{matches} is simpler and faster: 
#' usually faster than the best joins in other packages and thousands of times faster than the built in \code{\link{merge}}.
#' 
#' \code{all.x/all.y} correspond to the four types of database joins in the following way:
#' 
#' \describe{
#' \item{left}{\code{all.x=TRUE}, \code{all.y=FALSE}}
#' \item{right}{\code{all.x=FALSE}, \code{all.y=TRUE}}
#' \item{inner}{\code{all.x=FALSE}, \code{all.y=FALSE}}
#' \item{full}{\code{all.x=TRUE}, \code{all.y=TRUE}}
#' }
#' 
#' Note that \code{NA} values will match other \code{NA} values.
#' 
#' @param x vector.  The values to be matched.  Long vectors are not currently supported.
#' @param y vector.  The values to be matched.  Long vectors are not currently supported.
#' @param all.x logical; if \code{TRUE}, then each value in \code{x} will be included
#'  even if it has no matching values in \code{y}
#' @param all.y logical; if \code{TRUE}, then each value in \code{y} will be included
#'  even if it has no matching values in \code{x}
#' @param list logical.  If \code{TRUE}, the result will be returned as a list of vectors, each vector being the matching values in y.
#'  If \code{FALSE}, result is returned as a data frame with repeated values for each match.
#' @param indexes logical.  Whether to return the indices of the matches or the actual values.
#' @param nomatch the value to be returned in the case when no match is found. If not provided
#'  and \code{indexes=TRUE}, items with no match will be represented as \code{NA}.  If set to \code{NULL},
#'  items with no match will be set to an index value of \code{length+1}.  If
#'  {indexes=FALSE}, they will default to \code{NA}.
#' @export
#' @examples
#' one<-as.integer(1:10000)
#' two<-as.integer(sample(1:10000,1e3,TRUE))
#' system.time(a<-lapply(one, function (x) which(two %in% x)))
#' system.time(b<-matches(one,two,all.y=FALSE,list=TRUE))
#' 
#' one<-round(runif(1e3),3)
#' two<-round(runif(1e3),3)
#' system.time(a<-lapply(one, function (x) which(two %in% x)))
#' system.time(b<-matches(one,two,all.y=FALSE,list=TRUE))
#'  
#' one<-as.character(1:1e5)
#' two<-as.character(sample(1:1e5,1e5,TRUE))
#' system.time(b<-matches(one,two,list=FALSE))
#' system.time(c<-merge(data.frame(key=one),data.frame(key=two),all=TRUE))
#'  
#' \dontrun{
#' one<-as.integer(1:1000000)
#' two<-as.integer(sample(1:1000000,1e5,TRUE))
#' system.time(b<-matches(one,two,indexes=FALSE))
#' if(requireNamespace("dplyr",quietly=TRUE))
#'  system.time(c<-dplyr::full_join(data.frame(key=one),data.frame(key=two)))
#' if(require(data.table,quietly=TRUE))
#'  system.time(d<-merge(data.table(data.frame(key=one))
#'              ,data.table(data.frame(key=two))
#'              ,by='key',all=TRUE,allow.cartesian=TRUE))
#' 
#' one<-as.character(1:1000000)
#' two<-as.character(sample(1:1000000,1e5,TRUE))
#' system.time(a<-merge(one,two)) #Times out
#' system.time(b<-matches(one,two,indexes=FALSE))
#' if(requireNamespace("dplyr",quietly=TRUE))
#'  system.time(c<-dplyr::full_join(data.frame(key=one),data.frame(key=two)))#'
#' if(require(data.table,quietly=TRUE))
#' {
#'  system.time(d<-merge(data.table(data.frame(key=one))
#'              ,data.table(data.frame(key=two))
#'              ,by='key',all=TRUE,allow.cartesian=TRUE))
#'  identical(b[,1],as.character(d$key))
#' }
#' }
matches<-function(x,y,all.x=TRUE,all.y=TRUE,list=FALSE,indexes=TRUE,nomatch=NA)
{
  result<-.Call('matches',x,y)
  result<-data.frame(x=result[[1]],y=result[[2]])
  if(!all.y)
    result<-result[!is.na(result$x),]
  if(!all.x)
    result<-result[!is.na(result$y),]
  if(!indexes)
  {
    result$x<-x[result$x]
    result$y<-y[result$y]
  }
  else if(!is.null(nomatch)) 
  {
    result$x[result$x==length(x)+1]<-nomatch
    result$y[result$y==length(y)+1]<-nomatch
  }
  if(list)
    result<-tapply(result$y,result$x,function (z) z[!is.na(z)])
  return(result)
}

#'Extract/return parts of objects
#'
#'Alternative to built-in \code{\link{Extract}} or \code{[}.  Allows for extraction operations that are ambivalent to the data type of the object.
#'For example, \code{extract(x,i)} will work on lists, vectors, data frames, matrices, etc.  
#'
#'Extraction is 2-100x faster on data frames than with the built in operation - but does not preserve row names.
#'
#'@param x object from which to extract elements
#'@param i,j indices specifying elements to extract.  Can be \code{numeric}, \code{character}, or \code{logical} vectors.
#'@export
#'@examples
#'#Typically about twice as fast on normal subselections
#'orders<-data.frame(orderNum=1:1e5,
#'  sku=sample(1e3, 1e5, TRUE),
#'  customer=sample(1e4,1e5,TRUE))
#'a<-sample(1e5,1e4)
#' system.time(b<-orders[a,])
#'system.time(c<-extract(orders,a))
#'rownames(b)<-NULL
#'rownames(c)<-NULL
#'identical(b,c)
#'
#'#Speedup increases to 50-100x with oversampling 
#'a<-sample(1e5,1e6,TRUE)
#' system.time(b<-orders[a,])
#'system.time(c<-extract(orders,a))
#'rownames(b)<-NULL
#'rownames(c)<-NULL
#'identical(b,c)
#'
#'#Can create function calls that work for multiple data types
#'alist<-as.list(1:50)
#'avector<-1:50
#'extract(alist,1:5)
#'extract(avector,1:5)
#'extract(orders,1:5)#'
#'
#'\dontrun{
#'orders<-data.frame(orderNum=as.character(sample(1e5, 1e6, TRUE)),
#'  sku=sample(1e3, 1e6, TRUE),
#'  customer=sample(1e4,1e6,TRUE))
#'system.time(a<-sample(1e6,1e7,TRUE))
#' system.time(b<-orders[a,])
#'system.time(c<-extract(orders,a))
#'}
extract<-function(x,i=NULL,j=NULL)
{
  if(is.null(dim(x)))
  {
     x<-x[i]
    return(x)
  }
  else
    if(!is.null(j))
      x<-x[,j]
  if(!is.null(i))
  {
  if(is.data.frame(x))
    x<-as.data.frame(lapply(x,function (a) a[i]))
  else
    x<-x[i,]
  }
  return(x)
}  

# 
# #' @param x 
# #' @param f 
# #'@examples
# #'df<-data.frame(one=sample(4e4,1e6,TRUE),two=1:1000000,three=as.factor(sample(letters,1e6,TRUE)))
# #'system.time(a<-split(df,df$one))
# #'system.time(a<-split(as.matrix(df),df$one))
# #'system.time(b<-lapply(a,function (x) data.frame(matrix(x,ncol=2))))
# #'system.time({df2<-as.matrix(data.frame(lapply(df,as.factor))); c<-split(df2,df$one);d<-lapply(c,function (x) data.frame(matrix(x,ncol=2)))})
# #'system.time({factors<-df[,unlist(lapply(df,is.factor)),drop=FALSE];
# #'  nums<-df[,unlist(lapply(df,is.numeric)),drop=FALSE];
# #'  e<-split(as.matrix(factors),df$one);
# #'  f<-split(as.matrix(nums),df$one);
# #'  g<-Map(function(x,y) structure(data.frame(matrix(x,ncol=1),matrix(y,ncol=2)),names=c(colnames(e),colnames(f))),e,f);
# #'  })
# #'  system.time({factors<-colnames(df)[unlist(lapply(df,is.factor))]
# #'  nums<-colnames(df)[unlist(lapply(df,is.numeric))]
# #'  e<-split(as.matrix(df[,factors]),df$one);
# #'  f<-split(as.matrix(df[,nums]),df$one);
# #'  g<-Map(function(x,y) structure(data.frame(matrix(x,ncol=1),matrix(y,ncol=2)),names=c(factors,nums))[,colnames(df)],e,f);
# #'  })
# #'  system.time({factors<-colnames(df2)[unlist(lapply(df2,is.factor))]
# #'  nums<-colnames(df2)[unlist(lapply(df2,is.numeric))]
# #'  e<-split(as.matrix(df2[,factors]),df2$one);
# #'  f<-split(as.matrix(df2[,nums]),df2$one);
# #'  g<-Map(function(x,y) structure(data.frame(matrix(x,ncol=length(factors)),matrix(y,ncol=length(nums))),names=c(factors,nums))[,colnames(df2)],e,f);
# #'  })
# #'  
# #'  system.time(results<-lapply(df,function (x) split(as.matrix(x)),df$one))
# #'  nums<-df[,unlist(lapply(df,is.numeric)),drop=FALSE];
# #'  e<-split(as.matrix(factors),df$one);
# #'  f<-split(as.matrix(nums),df$one);
# #'  g<-Map(function(x,y) data.frame(matrix(x,ncol=1),matrix(y,ncol=2)),e,f)
# #'  })
# #'system.time(results<-lapply(df2,split,df2$one))
# #'system.time(results<-.Internal(mapply(data.frame,dots<-results,NULL)))
# split2<-function(x,f)
# {
#   factors<-colnames(x)[unlist(lapply(x,function (y) is.factor(y) || is.character(y)))]
#   nums<-colnames(x)[unlist(lapply(x,is.numeric))]
#   e<-split(as.matrix(x[,factors]),f);
#   f<-split(as.matrix(x[,nums]),f);
#   g<-Map(function(x,y) structure(data.frame(matrix(x,ncol=length(factors)),matrix(y,ncol=length(nums))),names=c(factors,nums))[,colnames(df2)],e,f);
# }