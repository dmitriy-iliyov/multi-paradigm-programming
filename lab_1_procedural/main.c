#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>


#define ALPHABET_POWER 26
#define SIZE 10z
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
        array[i] = rand() % 201;
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

double reley_distribution(double x, double sigma) {
    if (x < 0) return 0;
    return (1 - exp(-0.5 * pow(x / sigma, 2)));
}

double inverse_reley_distribution(double P, double sigma) {
    if (P < 0 || P > 1) return -1;
    if (P == 1) return -1;
    printf("pB = %f \n", P);
    return sigma * sqrt(-2 * log(1 - P));
}

void cutToIntervals() {
    double summ = 0;
    for (int i = 0; i < SIZE; i++) {
        summ += sorted_array[i] * sorted_array[i];
    }

    double sigma = sqrt(summ / (2 * SIZE));
    int interval_width = SIZE / ALPHABET_POWER;
    double interval[2];
    double buffer = sorted_array[0];
    double buffer_2 = 0;

    for (int i = 0; i < ALPHABET_POWER; i++) {
        interval[0] = buffer;
        double a = reley_distribution(interval[0], sigma);
        double b = inverse_reley_distribution(1.0/ALPHABET_POWER + a, sigma);
        printf("pA = %f ", a);
        printf("\n");
        printf("b = %f ", b);
        printf("\n");

        printf("%s", "----------------\n");

        interval[1] = b;
        buffer = interval[1];
        matrix_interval[i][0] = interval[0];
        matrix_interval[i][1] = interval[1];
    }
    matrix_interval[ALPHABET_POWER - 1][1] = sorted_array[SIZE - 1];

    printf("Intervals:\n");
    for (int i = 0; i < ALPHABET_POWER; i++) {
        printf("%c: [%f, %f]\n", alphabet[i], matrix_interval[i][0], matrix_interval[i][1]);
    }
    printf("\n");
}

void toCharArray(){
    for(int i = 0; i < size; i++){
        for(int j = 0; j < ALPHABET_POWER; j++){
            if(array[i] >= matrix_interval[j][0] &&  array[i] <= matrix_interval[j][1])
                char_array[i] = alphabet[j];
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

    if(FLAG == 1)
        valuesByUser();
    else
        valuesByRandom();

    printf("default array:\n");
    printIntArray(array, SIZE);

    sortArray();

    printf("sorted array:\n");
    printIntArray(sorted_array, SIZE);

    setAlphabet();
    printf("alphabet:\n");
    printCharArray(alphabet, ALPHABET_POWER);

    cutToIntervals();

    toCharArray();
    printf("char array:\n");
    printCharArray(char_array, SIZE);

    makeResultMatrix();
    printResultMatrix();

    return 0;
}