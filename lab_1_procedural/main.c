#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>


#define ALPHABET_POWER 10
#define SIZE 10
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

    double F_a, F_b;
    double shift = 0.0;
    double scale = 1.0;
    double interval_width = (sorted_array[size - 1] - sorted_array[0] + 3) / ALPHABET_POWER;
    double matrix_interval[ALPHABET_POWER][2];
    double interval[2];
    for (int i = 0; i < ALPHABET_POWER; ++i) {
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

//    double F_a, a, b;
//    double shift = 0.0;
//    double scale = 1.0;
//    if (array[0] <= shift) {
//        F_a = 0.5 * exp((array[0] - shift) / scale);
//    } else {
//        F_a =  1 - 0.5 * exp(-(array[0] - shift) / scale);
//    }
//    double probability = 1.0 / ALPHABET_POWER;
//    double matrix_interval[ALPHABET_POWER][2];
//    double interval[2];
//    for (int i = 0; i < ALPHABET_POWER; i++) {
//        if (F_a <= 1) {
//            a = scale * log(2 * F_a) + shift;
//        } else {
//            a = -scale * -log((1 - F_a) / 0.5) + shift;
//        }
//        double F_b = probability + F_a;
//        if (F_b <= 1) {
//            b = scale * log(2 * F_b) + shift;
//        } else {
//            b = -scale * -log((1 - F_b) / 0.5) + shift;
//        }
//        interval[0] = a;
//        interval[1] = b;
//        printf("Интервал [%.50lf, %f] [%.50lf, %f] : вероятность = %f\n", F_a, a, F_b, b,  probability);
//        for (int j = 0; j < 2; j++) {
//            matrix_interval[i][j] = interval[j];
//        }
//        a = 0.0;
//        F_a = F_b;
//        F_b = 0.0;
//    }
//
//    printf("intervals:\n");
//    for (int i = 0; i < ALPHABET_POWER; i++) {
//        for (int j = 0; j < 2; j++) {
//            printf("%f\t", matrix_interval[i][j]);
//        }
//        printf("\n");
//    }

    free(array);
    return 0;
}