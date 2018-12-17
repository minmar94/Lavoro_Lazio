#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "timing.h"
#include "R.h"
#define NROW 319
//#define NCOL 46

int main(int argc, char **argv){

  if(argc != 4){
    fprintf(stderr, "Numero di parametri non corretto!\n Inserire i seguenti parametri: file_in, ncol, file_out");
    exit(EXIT_FAILURE);
  }
  // Inizializzazione quantità
  FILE *f1;
  int tot1, tot2, tot_comm, i, j, k;
  float jaccard;
  //float MAT[NROW][NCOL];
  int NCOL = atoi(argv[2]);
  if(NCOL <= 0){
    fprintf(stderr, "Il numero di colonne deve essere strettamente positivo");
  }

  // Allocazione memoria matrice e controllo
  float *MAT = (float *)malloc(NROW * NCOL * sizeof(float));
  if(MAT == NULL){
    fprintf(stderr, "malloc fallita. \n");
    exit(EXIT_FAILURE);
  }

  // Controllo apertura del file
  f1 = fopen(argv[1], "r");
  if(f1 == NULL){
    fprintf(stderr,"Errore nell'apertura del file in lettura!");
    exit(EXIT_FAILURE);
  }

  TIMER_DEF;
  TIMER_START;
  // Salvo i valori in un array bidimensionale
  for(i = 0; i < NROW; i++){
    for(j = 0; j < NCOL; j++){
      fscanf(f1, "%f", &*(MAT + i*NCOL + j));
    }
  }
  fclose(f1);
  // Calcolo l'indice di Jaccard e lo salvo in un file di output
  // Controllo apertura del file
  f1 = fopen(argv[3], "w");
  if(f1 == NULL){
    fprintf(stderr,"Errore nell'apertura del file in scrittura!");
    exit(EXIT_FAILURE);
  }

  for(j = 0; j < NCOL; j++){
    for(k = 0; k < NCOL; k++){
      tot1 = tot2 = tot_comm = 0;
      if(j == k){
        fprintf(f1, "%d ", 0);
      } else {
        for(i = 0; i < NROW; i++){
          tot1 += (*(MAT + i*NCOL + j)>0);
          tot2 += (*(MAT + i*NCOL + k)>0);
          tot_comm += (*(MAT + i*NCOL + j)>0 && *(MAT + i*NCOL + k)>0);
        }
        jaccard = 1 - tot_comm/(1.0*(tot1+tot2-tot_comm));
        fprintf(f1, "%f ", jaccard);
      }
    }
    fprintf(f1, "\n");
  }
  fclose(f1);
  free(MAT);
  TIMER_STOP;
  printf("%f\n", TIMER_ELAPSED);
}
