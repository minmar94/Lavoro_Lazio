#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix J_index_cpp(NumericMatrix MAT) {
  // Inizializzazione quantità
  int NROW = 319;
  int NCOL = MAT.ncol();
  int tot1, tot2, tot_comm, i, j, k;
  double jaccard;
  NumericMatrix MATOUT(NCOL, NCOL);
  
  for(j = 0; j < NCOL; j++){
    for(k = 0; k < NCOL; k++){
      tot1 = tot2 = tot_comm = 0;
      // if(j == k){
      //   MATOUT(j,j) = 0;
      // } else {
        for(i = 0; i < NROW; i++){
          tot1 += (MAT(i,j)>0);
          tot2 += (MAT(i,k)>0);
          tot_comm += (MAT(i,j)>0 && MAT(i,k)>0);
        }
        jaccard = 1 - tot_comm/(1.0*(tot1+tot2-tot_comm));
        MATOUT(j,k) = jaccard;
      //}
    }
  }
  return(MATOUT);
}
