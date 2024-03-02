#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>


#define ALPHABET_POWER 4
#define SIZE 6
#define FLAG 0

int size;
int * array;
int * sorted_array;
char alphabet[ALPHABET_POWER];
char char_array[SIZE];
double matrix_interval[ALPHABET_POWER][2];
char result_matrix [ALPHABET_POWER + 1][ALPHABET_POWER + 1];

void valuesByUser(){
    printf("Enter count of values: ");
    scanf("%d", &size);
    array = (int*)malloc(size * sizeof(int));
    for (int i = 0; i < size; i++){
        printf("a[%d] = ", i);
        scanf("%d", &array[i]);
    }
}

void valuesByRandom(){
    size = SIZE;
    array = (int*)malloc(size * sizeof(int));
    srand(time(NULL));
    for (int i = 0; i < size; i++) {
        array[i] = rand() % 201 - 100;
    }
}

void sortArray(){
    sorted_array = (int*)malloc(size * sizeof(int));
    for (int i = 0; i < SIZE; i++)
        sorted_array[i] = array[i];
    for(int i = 1; i < size; i++){
        for(int k = i; k > 0 &&  sorted_array[k-1] > sorted_array[k]; k--){
            int tmp = sorted_array[k-1];
            sorted_array[k-1] = sorted_array[k];
            sorted_array[k] = tmp;
        }
    }
}

void setAlphabet(){
    for (int i = 0; i < ALPHABET_POWER; ++i) {
        alphabet[i] = 'A' + i;
    }
}

void cutToIntervals(){
    double interval_width = (sorted_array[size - 1] - sorted_array[0]) / (ALPHABET_POWER - 1);
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
        for (int j = 0; j < 2; j++)
            printf("%c %f ", alphabet[i], matrix_interval[i][j]);
        printf("\n");
    }
    printf("\n");
}

void intArrayToCharArray(){
    for(int i = 0; i < size; i++){
        for (int j = 0; j < ALPHABET_POWER; j++) {
            for (int k = 1; k < 2; k++) {
                if(array[i] >= matrix_interval[j][k - 1] && array[i] <= matrix_interval[j][k]){
                    char_array[i] = alphabet[j];
                }
            }
        }
    }
}

int findIndex(char a, char * charArray){
    int index = -1;
    for(int i = 0; i < ALPHABET_POWER; i++){
        if(a == charArray[i])
            index = i;
    }
    return index;
}

void makeResultMatrix(){
    for (int i = -1; i < ALPHABET_POWER; i++) {
        for (int j = -1; j < ALPHABET_POWER; j++) {
            if (i == -1) {
                if (j == -1)
                    result_matrix[i][j] = ' ';
                else
                    result_matrix[i][j] = alphabet[j];
            } else {
                if (j == -1)
                    result_matrix[i][j] = alphabet[i];
                else{
                    result_matrix[i][j] = '0';
                }
            }
        }
    }
    for(int i = 0; i < size; i++){
        if(i + 1 < size){
            int current_index = findIndex(char_array[i], alphabet);
            int next_index = findIndex(char_array[i + 1], alphabet);
            if(current_index != -1 && next_index != -1){
                result_matrix[current_index][next_index] = result_matrix[current_index][next_index] + 1;
            }
        }
    }
}

void printResultMatrix(){
    printf("result_matrix:\n");
    for (int i = -1; i < ALPHABET_POWER; i++) {
        for (int j = -1; j < ALPHABET_POWER; j++) {
            printf("%c ", result_matrix[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

void printIntArray(int * _array, int size){
    for (int i = 0; i < size; i++)
        printf("%d ", _array[i]);
    printf("\n");
    printf("\n");
}

void printCharArray(char * charArray, int size){
    for (int i = 0; i < size; ++i)
        printf("%c ", charArray[i]);
    printf("\n");
    printf("\n");
}


int main (){

    if(FLAG == 1){
        valuesByUser();
    }
    else{
        valuesByRandom();
    }

    printf("default array:\n");
    printIntArray(array, SIZE);

    sortArray();

    printf("sorted array:\n");
    printIntArray(sorted_array, SIZE);

    setAlphabet();
    printf("alphabet:\n");
    printCharArray(alphabet, ALPHABET_POWER);

    cutToIntervals();

    intArrayToCharArray();
    printf("char array:\n");
    printCharArray(char_array, SIZE);

    free(array);

    makeResultMatrix();
    printResultMatrix();

    return 0;
}