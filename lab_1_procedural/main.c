#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>


#define ALPHABET_POWER 4
#define SIZE 6
#define FLAG 0


double laplace_cdf(double x, double mu, double b) {
    if (x < mu) {
        return 0.5 * exp((x - mu) / b);
    } else {
        return 1 - 0.5 * exp(-(x - mu) / b);
    }
}


int main (){
    int size;
    int * array;

    if(FLAG == 1){
        printf("Enter count of values: ");
        scanf("%d", &size);
        array = (int*)malloc(size * sizeof(int));
        for (int i = 0; i < size; i++){
            printf("a[%d] = ", i);
            scanf("%d", &array[i]);
        }
    }
    else{
        size = SIZE;
        array = (int*)malloc(size * sizeof(int));
        srand(time(NULL));
        for (int i = 0; i < size; i++) {
            array[i] = rand() % 201 - 100;
        }
    }

    printf("default array:\n");
    for (int i = 0; i < size; i++)
        printf("%d ", array[i]);
    printf("\n");

    int *sorted_array = (int*)malloc(size * sizeof(int));;
    for (int i = 0; i < SIZE; i++) {
        sorted_array[i] = array[i];
    }

    for(int i = 1; i < size; i++){
        for(int k=i; k>0 &&  sorted_array[k-1] > sorted_array[k]; k--){
            int tmp = sorted_array[k-1];
            sorted_array[k-1] = sorted_array[k];
            sorted_array[k] = tmp;
        }
    }

    printf("sorted array:\n");
    for (int i = 0; i < size; i++)
        printf("%d ", sorted_array[i]);
    printf("\n");

    char alphabet[ALPHABET_POWER];
    for (int i = 0; i < ALPHABET_POWER; ++i) {
        alphabet[i] = 'A' + i;
    }

    printf("alphabet:\n");
    for (int i = 0; i < ALPHABET_POWER; ++i) {
        printf("%c ", alphabet[i]);
    }
    printf("\n");

    double interval_width = (sorted_array[size - 1] - sorted_array[0]) / (ALPHABET_POWER - 1);
    double matrix_interval[ALPHABET_POWER][2];
    double interval[2];
    for (int i = 0; i < ALPHABET_POWER; i++) {
        double a = sorted_array[0] + i * interval_width;
        double b = sorted_array[0] + (i + 1) * interval_width;
        interval[0] = a;
        interval[1] = b;
        for (int j = 0; j < 2; j++) {
            matrix_interval[i][j] = interval[j];
        }
    }

    printf("intervals:\n");
    for (int i = 0; i < ALPHABET_POWER; i++) {
        for (int j = 0; j < 2; j++) {
            printf("%c %f ", alphabet[i], matrix_interval[i][j]);
        }
        printf("\n");
    }

    char char_array[size];
    for(int i = 0; i < size; i++){
        for (int j = 0; j < ALPHABET_POWER; j++) {
            for (int k = 1; k < 2; k++) {
                if(array[i] >= matrix_interval[j][k - 1] && array[i] <= matrix_interval[j][k]){
                    char_array[i] = alphabet[j];
                }
            }
        }
    }

    printf("char array:\n");
    for (int i = 0; i < size; ++i) {
        printf("%c ", char_array[i]);
    }
    printf("\n");
    printf("\n");

    char result_matrix [ALPHABET_POWER + 1][ALPHABET_POWER + 1];
    for (int i = -1; i < ALPHABET_POWER; i++) {
        for (int j = -1; j < ALPHABET_POWER; j++) {
            if (i == -1) {
                if (j == -1)
                    result_matrix[i][j] = ' ';
                else{
                    result_matrix[i][j] = alphabet[j];
                }
            } else {
                if (j == -1)
                    result_matrix[i][j] = alphabet[i];
                else{
                    result_matrix[i][j] = '0';

                }
            }
        }
    }

    printf("result_matrix:\n");
    for (int i = -1; i < ALPHABET_POWER; i++) {
        for (int j = -1; j < ALPHABET_POWER; j++) {
            printf("%c ", result_matrix[i][j]);
        }
        printf("\n");
    }

    free(array);
    return 0;
}