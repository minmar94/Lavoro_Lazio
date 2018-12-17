#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
//#include "timing.h"
#include "R.h"
#define NROW 319
//#define NCOL 46

void J_index(int *NCOL, double *MAT){

  // Inizializzazione quantità
  int tot1, tot2, tot_comm, i, j, k;
  double jaccard;
  if(*NCOL <= 0){
    Rprintf("Il numero di colonne deve essere strettamente positivo");
    exit(EXIT_FAILURE);
  }
  // for (i=0; i<NROW; i++){
  //   MAT[i] = (double *)malloc((*NCOL) * sizeof(double));
  // }
  // double *OutMat = NULL;
  // OutMat = calloc(NROW * (*NCOL),sizeof (double));
  //MAT = (double *)malloc(NROW * (*NCOL) * sizeof(double));
  for(j = 0; j < *NCOL; j++){
    for(k = 0; k < *NCOL; k++){
      tot1 = tot2 = tot_comm = 0;
      if(j == k){
        //*(OutMat + i*(*NCOL) + k) = 0;
        *(MAT + k*(*NCOL) + k) = 0;
      } else {
        for(i = 0; i < NROW; i++){
          tot1 += (*(MAT + i*(*NCOL) + j)>0);
          tot2 += (*(MAT + i*(*NCOL) + k)>0);
          tot_comm += (*(MAT + i*(*NCOL) + j)>0 && *(MAT + i*(*NCOL) + k)>0);
        }
        jaccard = 1 - tot_comm/(1.0*(tot1+tot2-tot_comm));
        *(MAT + j*(*NCOL) + k) = jaccard;
      }
    }
    //printf("\n");
  }
//
// de-allocate the local results vector.
//
 //free(OutMat);
  //free(MAT);
}
