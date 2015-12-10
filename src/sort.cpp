#include <R.h>
#include <Rdefines.h>
#include <algorithm>
#include <string.h>
#include <vector>
#include <numeric>
#include <iostream>
// #include <Rcpp.h>
// using namespace Rcpp;

struct CMP_CHAR {
  bool operator()(SEXP x, SEXP y) {
    return strcmp(CHAR(x), CHAR(y)) < 0;
  }
} cmp_char;


// [[Rcpp::export]]
SEXP sortcpp(SEXP x) {
  
  //std::cout << TYPEOF(x);
  x = PROTECT(Rf_duplicate(x));
  switch(TYPEOF(x))
  {
    case INTSXP:
      std::sort(INTEGER(x),INTEGER(x)+LENGTH(x));
      break;
    case REALSXP:
      std::sort(REAL(x),REAL(x)+LENGTH(x));
      break;
    case LGLSXP:
      std::sort(LOGICAL(x),LOGICAL(x)+LENGTH(x));
      break;
    case STRSXP:
      std::sort(STRING_PTR(x), STRING_PTR(x) + LENGTH(x), cmp_char);
      break;
    default:
      error_return("Unsupported type for sort.")
      ;
  }
  UNPROTECT(1);
  return x;
}



struct CMP_INT {
  int* start;
  CMP_INT(int* start) : start(start) {};
  bool operator()(int x, int y) {
    return *(start+x-1) - *(start+y-1) < 0;
  }
};


// [[Rcpp::export]]
SEXP ordercpp(SEXP x) {
  
  //std::cout << TYPEOF(x);
  //int* xpoint = INTEGER(x); //Pointer to int array of input x
  
  SEXP result = PROTECT(allocVector(INTSXP,LENGTH(x)));
  int* respoint = INTEGER(result);
  int* start = &INTEGER(x)[0];

  for (int i = 0; i < LENGTH(x); i++)
  {
    //std::cout << *(start+i);
    respoint[i] = i+1; //shift compensates for difference between R and C indexing
  }
  std::sort(INTEGER(result),INTEGER(result)+LENGTH(x), CMP_INT(start));

  //   std::vector<int> v;  //   v.assign(respoint,respoint + LENGTH(result));  //int* a = &v[0];
  
  UNPROTECT(1);
  return(result);

}

struct CMP_CHAR2 {
  SEXP* start;
  CMP_CHAR2(SEXP* start) : start(start) {};
  bool operator()(int x, int y) {
    return strcmp(CHAR(*(start+x-1)),CHAR(*(start+y-1))) < 0;
  }
};


// [[Rcpp::export]]
SEXP ordercpp2(SEXP x) {
  
  //std::cout << TYPEOF(x);
  //int* xpoint = INTEGER(x); //Pointer to int array of input x
  
  SEXP result = PROTECT(allocVector(INTSXP,LENGTH(x)));
  int* respoint = INTEGER(result);
  SEXP* start = &STRING_PTR(x)[0];
  
  for (int i = 0; i < LENGTH(x); i++)
  {
    //std::cout << *(start+i);
    respoint[i] = i+1; //shift compensates for difference between R and C indexing
  }
  std::sort(INTEGER(result),INTEGER(result)+LENGTH(x), CMP_CHAR2(start));
  
  //   std::vector<int> v;  //   v.assign(respoint,respoint + LENGTH(result));  //int* a = &v[0];
  
  UNPROTECT(1);
  return(result);
  
}
// 
// struct CMP_CHAR2 {
//   SEXP* start;
//   CMP_CHAR2(SEXP* start) : start(start) {};
//   bool operator()(int x, int y) {
//     return strcmp(CHAR(*(start+x-1)),CHAR(*(start+y-1))) < 0;
//   }
// };
// 
// 
// // [[Rcpp::export]]
// SEXP ordercpp2(SEXP x) {
//   
//   //std::cout << TYPEOF(x);
//   //int* xpoint = INTEGER(x); //Pointer to int array of input x
//   
//   SEXP result = PROTECT(allocVector(INTSXP,LENGTH(x)));
//   int* respoint = INTEGER(result);
//   SEXP* start = &STRING_PTR(x)[0];
//   
//   for (int i = 0; i < LENGTH(x); i++)
//   {
//     //std::cout << *(start+i);
//     respoint[i] = i+1; //shift compensates for difference between R and C indexing
//   }
//   std::sort(INTEGER(result),INTEGER(result)+LENGTH(x), CMP_CHAR2(start));
//   
//   //   std::vector<int> v;  //   v.assign(respoint,respoint + LENGTH(result));  //int* a = &v[0];
//   
//   UNPROTECT(1);
//   return(result);
//   
// }



// [[Rcpp::export]]
SEXP ordercpp3(SEXP x) {
  
  //std::cout << TYPEOF(x);
  //int* xpoint = INTEGER(x); //Pointer to int array of input x
  
  SEXP result = PROTECT(allocVector(INTSXP,LENGTH(x)));
  int* respoint = INTEGER(result);
  for (int i = 0; i < LENGTH(x); i++)
  {
    //std::cout << *(start+i);
    respoint[i] = i+1; //shift compensates for difference between R and C indexing
  }
  switch(TYPEOF(x))
  {
  case INTSXP:
  {
    int* start = &INTEGER(x)[0];
    std::sort(INTEGER(result),INTEGER(result)+LENGTH(x), CMP_INT(start));
    break;
  }
  case REALSXP:
    std::sort(REAL(x),REAL(x)+LENGTH(x));
    break;
  case LGLSXP:
    std::sort(LOGICAL(x),LOGICAL(x)+LENGTH(x));
    break;
  case STRSXP:
  {
    SEXP* start = &STRING_PTR(x)[0];
    std::sort(INTEGER(result),INTEGER(result)+LENGTH(x), CMP_CHAR2(start));
    break;
  }
  default:
    error_return("Unsupported type for sort.")
    ;
  }
  
  //   std::vector<int> v;  //   v.assign(respoint,respoint + LENGTH(result));  //int* a = &v[0];
  
  UNPROTECT(1);
  return(result);
  
}

