#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <stdio.h>

/*
 * 1. random data -> DATA_ENTERING_FLAG 0
 * 2. user data -> DATA_ENTERING_FLAG 1
 * 3. file data -> DATA_ENTERING_FLAG 2
 */

#define ALPHABET_POWER 26
#define DATA_ENTERING_FLAG 2
#define DEBUGGING_FLAG 0
int size = 4;

char filename[] = "/Users/Sayner/github_repos/multi-paradigm-programming/unit_work/mpp_get_data_script/files/f_data.txt";
double * array;
char alphabet[ALPHABET_POWER];
double * sorted_array;
double matrix_interval[ALPHABET_POWER][2];
char * char_array;
int result_matrix [ALPHABET_POWER + 1][ALPHABET_POWER + 1];


// values generating

void valuesByUser(){
    printf("Enter count of values: ");
    scanf("%d", &size);
    array = (double*)malloc(size * sizeof(double));
    for (int i = 0; i < size; i++){
        printf("a[%d] = ", i);
        scanf("%lf", &array[i]);
    }
}

void valuesByRandom(){
    array = (double*)malloc(size * sizeof(double));
    srand(time(NULL));
    for (int i = 0; i < size; i++) {
        array[i] = rand() % 201;
    }
}

int countLinesInFile(){
    int lineCount = 0;
    FILE *file;
    char c;

    file = fopen(filename, "r");

    if (file == NULL) {
        perror("file can't be opened\n");
        return -1;
    }
    while ((c = fgetc(file)) != EOF) {
        if (c == '\n') {
            lineCount++;
        }
    }

    fclose(file);
    return lineCount;
}

void valuesFromFile(){
    array = (double*)malloc(size * sizeof(double));
    char buffer[256];
    int count = 0;
    size = countLinesInFile();
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        perror("file can't be opened\n");
    }

    while (fgets(buffer, sizeof(buffer), file) != NULL) {
        if (count < size) {
            array[count] = atof(buffer);
            count++;
        } else {
            perror("maximum number of numbers exceeded \n");
            break;
        }
    }

    fclose(file);

    if (count == 0) {
        printf("file is empty or data not found.\n");
    }
}

// prepare data

void sortArray(){
    sorted_array = (double*)malloc(size * sizeof(double));
    for (int i = 0; i < size; i++)
        sorted_array[i] = array[i];
    for(int i = 1; i < size; i++){
        for(int k = i; k > 0 &&  sorted_array[k-1] > sorted_array[k]; k--){
            double tmp = sorted_array[k-1];
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

// cut to intervals

void func_to_debugging_intervals_cutting(double a, double pA, double b, double pB){
    printf("a : %f\n", a);
    printf("pA : %f\n", pA);
    printf("pB : %f\n", pB);
    printf("b : %f\n", b);
    printf("________________________________\n");
}

double reley_distribution(double x, double sigma) {
    if (x < 0) return 0;
    return (1 - exp(-0.5 * pow(x / sigma, 2)));
}

double inverse_reley_distribution(double P, double sigma) {
    if (P < 0 || P > 1) return -1;
    if (P == 1) return -1;
    return sigma * sqrt(-2 * log(1 - P));
}

void cutToIntervals() {
    double summ = 0;
    for (int i = 0; i < size; i++) {
        summ += sorted_array[i] * sorted_array[i];
    }

    double sigma = sqrt(summ / (2 * size));
    printf("sigma: %f\n\n", sigma);
    double interval[2];
    double a = sorted_array[0];
    for (int i = 0; i < ALPHABET_POWER; i++) {
        interval[0] = a;
        double pA = reley_distribution(interval[0], sigma);
        double pB = 1.0/ALPHABET_POWER + pA;
        double b = inverse_reley_distribution(pB, sigma);
        interval[1] = b;
        if(DEBUGGING_FLAG == 1)
            func_to_debugging_intervals_cutting(interval[0], pA, interval[1], pB);
        a = interval[1];
        matrix_interval[i][0] = interval[0];
        matrix_interval[i][1] = interval[1];
    }
    matrix_interval[ALPHABET_POWER - 1][1] = sorted_array[size - 1];
    if(matrix_interval[ALPHABET_POWER - 1][0] > matrix_interval[ALPHABET_POWER - 1][1]){
        double interval_width = (matrix_interval[ALPHABET_POWER - 1][1] - matrix_interval[ALPHABET_POWER - 2][0])/2;
        matrix_interval[ALPHABET_POWER - 2][1] = matrix_interval[ALPHABET_POWER - 2][0] + interval_width;
        matrix_interval[ALPHABET_POWER - 1][0] = matrix_interval[ALPHABET_POWER - 2][1];
    }

    printf("intervals:\n");
    for (int i = 0; i < ALPHABET_POWER; i++) {
        printf("%c: [%f, %f]\n", alphabet[i], matrix_interval[i][0], matrix_interval[i][1]);
    }
    printf("\n");
}

void toCharArray(){
    char_array = (char*)malloc(size * sizeof(char));
    for(int i = 0; i < size; i++){
        for(int j = 0; j < ALPHABET_POWER; j++){
            if(array[i] >= matrix_interval[j][0] &&  array[i] <= matrix_interval[j][1]){
                char_array[i] = alphabet[j];
                break;
            }
        }
    }
}

// making result matrix

int findIndex(char a, char * charArray){
    int index = -1;
    for(int i = 0; i < ALPHABET_POWER; i++){
        if(a == charArray[i])
            index = i;
    }
    return index;
}

void makeResultMatrix(){
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

// prints

void printResultMatrix() {
    printf("result_matrix:\n");
    printf("%4c ", ' ');
    for(int i=0; i < ALPHABET_POWER; i++){
        printf("%4c ", alphabet[i]);
    }
    printf("\n");
    for (int i = 0; i < ALPHABET_POWER; i++) {
        for (int j = 0; j < ALPHABET_POWER; j++) {
            if(j == 0){
                printf("%4c ", alphabet[i]);
            }
            printf("%4d ", result_matrix[i][j]);
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

void printDoubleArray(double * _array, int size){
    for (int i = 0; i < size; i++)
        printf("%.2f ", _array[i]);
    printf("\n");
    printf("\n");
}

void printCharArray(char *charArray, int size){
    for (int i = 0; i < size; ++i)
        printf("%c ", charArray[i]);
    printf("\n");
    printf("\n");
}


int main (){

    clock_t start, end;
    double cpu_time_used;
    start = clock();

    int CORRECT_DATA_FLAG = 0;
    if(DATA_ENTERING_FLAG == 1){
        valuesByUser();
        if (size > 0 && ALPHABET_POWER > 0)
            CORRECT_DATA_FLAG = 1;
    }else if(DATA_ENTERING_FLAG == 2){
        valuesFromFile();
        if(ALPHABET_POWER == 26 && size > 0)
            CORRECT_DATA_FLAG = 1;
    }else{
        valuesByRandom();
        if(ALPHABET_POWER > 0 && size > 0)
            CORRECT_DATA_FLAG = 1;
    }
    if(CORRECT_DATA_FLAG == 1){
        printf("default array:\n");
        printDoubleArray(array, size);

        sortArray();
        printf("sorted array:\n");
        printDoubleArray(sorted_array, size);

        setAlphabet();
        printf("alphabet:\n");
        printCharArray(alphabet, ALPHABET_POWER);

        cutToIntervals();

        toCharArray();
        printf("char array:\n");
        printCharArray(char_array, size);

        makeResultMatrix();
        printResultMatrix();
    }else
        printf("incorrect data.");

    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("%f secs\n", cpu_time_used);
    return 0;
}