#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM_ASCII_VALS 128
#define MAX_ASCII_VAL (NUM_ASCII_VALS - 1)

typedef char char_string[2];

char_string char_strings[NUM_ASCII_VALS];

void init_char_strings() {
    for (int i = 0; i < NUM_ASCII_VALS; ++i) {
        char_strings[i][0] = i;
    }
}

void print(const char * s) {
    fputs(s, stdout);
}

void flush() {
    fflush(stdout);
}

char * getchar_() {
    int c = getchar();
    if (c == EOF || c > MAX_ASCII_VAL) {
        return "";
    }
    if (!char_strings[MAX_ASCII_VAL][0]) {
        init_char_strings();
    }
    return (char *)(char_strings + c);
}

int ord(char * input) {
    if (!(*input)) {
        return -1;
    }
    return input[0];
}

char * chr(int code) {
    if (code < 1 || code > MAX_ASCII_VAL) {
        fprintf(stderr, "chr(%d) is out of range\n", code);
        exit(EXIT_FAILURE);
    }
    return (char *)(char_strings + code);
}

int size(char * input) {
    return strlen(input);
}

char * substring(char * s, int first, int n) {
    if (first < 0 || first + n > strlen(s)) {
        fprintf(stderr, "substring(%s, %d, %d) is out of range",
                s, first, n);
        exit(EXIT_FAILURE);
    }
    char * result = malloc(n + 1);
    strncpy(result, s + first, n);
    result[n] = '\0';
    return result;
}

char * concat(char * s1, char * s2) {
    int len1 = strlen(s1);
    if (!len1) {
        return s2;
    }
    int len2 = strlen(s2);
    if (!len2) {
        return s1;
    }
    char * result = malloc(len1 + len2 + 1);
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}

int not(int i) {
    return i == 0;
}

