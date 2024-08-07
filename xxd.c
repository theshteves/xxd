#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 16

#define MAX_MATCHES 2048
#define MAX_STRING_LEN 2048


char* regex_replace(const char* pattern, const char* replacement, const char* string) {
    regex_t regex;
    regmatch_t matches[MAX_MATCHES];
    char* result;
    size_t len_result = 0;
    int offset = 0;

    if (regcomp(&regex, pattern, REG_EXTENDED) != 0) {
        fprintf(stderr, "Could not compile regex\n");
        return NULL;
    }

    result = malloc(MAX_STRING_LEN);
    if (result == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        regfree(&regex);
        return NULL;
    }
    result[0] = '\0';

    while (regexec(&regex, string + offset, MAX_MATCHES, matches, 0) == 0) {
        strncat(result, string + offset, matches[0].rm_so);
        strcat(result, replacement);
        offset += matches[0].rm_eo;
        len_result = strlen(result);

        if (len_result >= MAX_STRING_LEN - 1) {
            fprintf(stderr, "Result string too long\n");
            free(result);
            regfree(&regex);
            return NULL;
        }
    }

    strcat(result, string + offset);
    regfree(&regex);
    return result;
}


void dump_row(int address, unsigned char *buffer, size_t bytes_read) {

    printf("%08x: ", address);

    // Dump hexadecimal
    for (int i = 1; i < bytes_read; i += 2) {
        printf("%02x%02x ", buffer[i - 1], buffer[i]);
    }

    int surplus = BUFFER_SIZE - bytes_read;
    if (surplus > 0) {
        for (int i = 0; i < surplus; i += 2) {
            printf("     ");
        }
    }

    // Dump formatted preview
    printf(" ");
    char *formattedBuffer = regex_replace((const char *)"[^ -~]", (const char *)".", (const char *)buffer);
    for (int i = 0; i < bytes_read; i++) {
        printf("%c", formattedBuffer[i]);
    }
    printf("\n");
}


int main(int argc, char *argv[]) {
    FILE *input_file = stdin;
    unsigned char buffer[BUFFER_SIZE];
    size_t bytes_read;

    // Read from file if provided & is valid
    if (argc > 1) {
        input_file = fopen(argv[1], "rb");
        if (input_file == NULL) {
            fprintf(stderr, "Error opening file: %s\n", argv[1]);
            return 1;
        }
    }

    // Read input in chunks of BUFFER_SIZE bytes
    int address = 0;
    while ((bytes_read = fread(buffer, 1, BUFFER_SIZE, input_file)) > 0) {
        dump_row(address, buffer, bytes_read);
        address += BUFFER_SIZE;
    }

    // Close the file if we opened one
    if (input_file != stdin) {
        fclose(input_file);
    }

    return 0;
}
