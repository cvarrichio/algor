#include <R.h>
#include <Rdefines.h>
#include <algorithm>
#include <string>
#include <vector>
#include <numeric>
#include <iostream>

struct CMP_CHAR {
  bool operator()(SEXP x, SEXP y) {
    return strcmp(CHAR(x), CHAR(y)) < 0;
  }
} cmp_char;


// [[Rcpp::export]]
extern "C" SEXP sortcpp(SEXP x) {
  
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
      UNPROTECT(1);
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

struct CMP_REAL {
  double* start;
  CMP_REAL(double* start) : start(start) {};
  bool operator()(int x, int y) {
    return *(start+x-1) - *(start+y-1) < 0;
  }
};

struct CMP_CHAR2 {
  SEXP* start;
  CMP_CHAR2(SEXP* start) : start(start) {};
  bool operator()(int x, int y) {
    return strcmp(CHAR(*(start+x-1)),CHAR(*(start+y-1))) < 0; // shift compensates for difference between R and C indexing
  }
};


// [[Rcpp::export]]
extern "C" SEXP ordercpp(SEXP x) {
  
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
  {
    double* start = &REAL(x)[0];
    std::sort(INTEGER(result),INTEGER(result)+LENGTH(x), CMP_REAL(start));
    break;
  }
  case LGLSXP:
  {
    int* start = &INTEGER(x)[0];
    std::sort(INTEGER(result),INTEGER(result)+LENGTH(x), CMP_INT(start));
    break;
  }
  case STRSXP:
  {
    SEXP* start = &STRING_PTR(x)[0];
    std::sort(INTEGER(result),INTEGER(result)+LENGTH(x), CMP_CHAR2(start));
    break;
  }
  default:
    UNPROTECT(1);
    error_return("Unsupported type for sort.")
    ;
  }
  
  //   std::vector<int> v;  //   v.assign(respoint,respoint + LENGTH(result));  //int* a = &v[0];
  
  UNPROTECT(1);
  return(result);
  
}



void internalOrder(int* index,SEXP x)
{
  for (int i = 0; i < LENGTH(x); i++)
  {
    //std::cout << *(start+i);
    index[i] = i+1; //shift compensates for difference between R and C indexing
  }
  switch(TYPEOF(x))
  {
  case INTSXP:
  {
    int* start = &INTEGER(x)[0];
    std::sort(index,index+LENGTH(x), CMP_INT(start));
    break;
  }
  case REALSXP:
  {
    double* start = &REAL(x)[0];
    std::sort(index,index+LENGTH(x), CMP_REAL(start));
    break;
  }
  case LGLSXP:
  {
    int* start = &INTEGER(x)[0];
    std::sort(index,index+LENGTH(x), CMP_INT(start));
    break;
  }
  case STRSXP:
  {
    SEXP* start = &STRING_PTR(x)[0];
    std::sort(index,index+LENGTH(x), CMP_CHAR2(start));
    break;
  }
  default:
    UNPROTECT(1);
    error("Unsupported type for sort.")
      ;
  }
}

// [[Rcpp::export]]
extern "C" SEXP ordercpp2(SEXP x) {
  
  //std::cout << TYPEOF(x);
  //int* xpoint = INTEGER(x); //Pointer to int array of input x
  SEXP result = PROTECT(allocVector(INTSXP,LENGTH(x)));
  int* respoint = INTEGER(result);
  internalOrder(respoint,x);
  
  //   std::vector<int> v;  //   v.assign(respoint,respoint + LENGTH(result));  //int* a = &v[0];
  
  UNPROTECT(1);
  return(result);
  
}

// [[Rcpp::export]]
extern "C" SEXP matches(SEXP a, SEXP b)
{
  int alength = LENGTH(a);
  int blength = LENGTH(b);
  SEXP sortedA = PROTECT(allocVector(INTSXP,alength));
  int* apoint = INTEGER(sortedA);
  internalOrder(apoint,a);
  SEXP sortedB = PROTECT(allocVector(INTSXP,blength));
  int* bpoint = INTEGER(sortedB);
  internalOrder(bpoint,b);
  std::vector<int> indexsA;
  indexsA.reserve(alength);
  std::vector<int> indexsB;
  indexsB.reserve(blength);
  int a1 =1, a2 = 1, b1 = 1, b2 = 1;
  //
  int* astart = &INTEGER(a)[0];
  int* bstart = &INTEGER(b)[0];
  
  while (a2 <= alength || b2 <= blength)
  {
    //if a1==a2 and  *(astart+a2)!=*(bstart+b1), then there is no match.
    std::cout << alength << std::endl;
    std::cout << a2 << ';';
    std::cout << b2 << std::endl;
    while(*(astart+a2-1)==*(bstart+b2-1) && a2<=alength && b2<=blength)
    {
      indexsA.push_back(a2);
      indexsB.push_back(b2);
      b2++;
    }
    if(b1!=b2)
    {
      if(a2<alength)
      {
        a2++;
        if(*(astart+a2-1)==*(astart+a1-1))
          b2=b1;
        
        else
          b1=b2;
        
      }
      else
        b1=b2;
      a2=++a1;
      continue;
    }
    std::cout << a2 << ';';
    std::cout << b2 << std::endl;
    if(*(astart+a2-1)<*(bstart+b2-1) || b2>blength)
    {
      
      indexsA.push_back(a2);
      indexsB.push_back(blength+1);
      
        a1=++a2;
        continue;
    }
    std::cout << a2 << ';';
    std::cout << b2 << std::endl;
    if(*(astart+a2-1)>*(bstart+b2-1) || a2>alength)
    {
      
      indexsA.push_back(alength+1);
      indexsB.push_back(b2);
        b1=++b2;
        continue;
    }
    
//     
//     //if acmp==bcmp, 1.add acmp to indexsA 2. increase b2 3. compare again for b2
//     b2++;
//     //when loop is compl
//     a2++;
//     a1=a2;
//     //if a2==a1, then b2<-b1. repeat
  }
  //Unfortunate overhead needed to convert vector to SEXP
  SEXP result1 = PROTECT(allocVector(INTSXP,indexsA.size()));
  SEXP result2 = PROTECT(allocVector(INTSXP,indexsB.size()));
  int* respoint1 = INTEGER(result1);
  int* respoint2 = INTEGER(result2);
  //Will this work?  If so, then we could probably dispense with allocating new vectors above (and instead allocate of size 0)
  //respoint1 = &indexsA.front();
  //respoint1 = &indexsB.front();
  std::copy(indexsA.begin(),indexsA.end(),respoint1);
  std::copy(indexsB.begin(),indexsB.end(),respoint2);
  SEXP combined = PROTECT(allocVector(VECSXP,2));
  SET_VECTOR_ELT(combined,0,result1);
  SET_VECTOR_ELT(combined,1,result2);
  UNPROTECT(5);
  return combined;
  
}


