#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdint.h>
#include <string.h>
#include "dolphin_rc.h"

struct string {int64_t len; char * contents; };

struct string dolphin_rc_empty_string = {0, NULL};

extern int64_t dolphin_fun_main();

struct string **cmd_args = NULL;

struct string **get_cmd_args(){
  return cmd_args;
}

int64_t string_length (struct string* s) {
    return s->len;
}

void *raw_allocate_on_heap(int32_t size){
    if(size < 0){
        fprintf(stderr, "Internal error: allocation of element negative size!\n");
        exit(1);
    }
    if(size == 0){
        return NULL;
    }
    void *res = malloc(size);
    if(res == NULL){
        fprintf(stderr, "Runtime error: memory allocation failed. This is likely the result of running out of memory!\n");
        exit(1);
    }
    return res;
}

void* allocate_record(int32_t size) {
    return raw_allocate_on_heap(size == 0 ? 1 : size );
}

void* allocate_array(int32_t size, int64_t numelems, void* contents){
    if(numelems < 0){
        fprintf(stderr, "Runtime error: the program attempted to create an array of negative length!\n");
        exit(1);
    }
    if(size < 0){
        fprintf(stderr, "Internal error: the attempted to create an array with elements of negative size!\n");
        exit(1);
    }
    int size_metadata = dolphin_rc_compute_array_length_size();
    if(size_metadata < 0){
        fprintf(stderr, "Internal error: the attempted to create an array with negative size for metadata!\n");
        exit(1);
    }
    if(numelems > (INT32_MAX - size_metadata) / size){
        fprintf(stderr, "Runtime error: array is too large to allocate!\n");
        exit(1);
    }
    int32_t numbytes = size_metadata + size * numelems;
    if(numbytes == 0){
        return NULL;
    }
    void *arr = raw_allocate_on_heap(numbytes);
    arr += size_metadata;
    void *ptr = arr;
    for(int32_t i = 0; i < numelems; i++){
        memcpy(ptr, contents, size);
        ptr += size;
    }
    dolphin_rc_set_array_length(arr, numelems);
    return arr;
}

void report_error_division_by_zero(){
    fprintf(stderr, "Runtime error: division by zero!\n");
    exit(1);
}

void report_error_nil_access(){
    fprintf(stderr, "Runtime error: attempt to access nil!\n");
    exit(1);
}

void report_error_array_index_out_of_bounds(){
    fprintf(stderr, "Runtime error: array index out of bounds!\n");
    exit(1);
}

int64_t compare_strings(struct string *s1, struct string *s2){
    int64_t flipped = 1;
    if (s1->len > s2->len){
        struct string *s = s1;
        s1 = s2;
        s2 = s;
        flipped = -1;
    }
    // from now on we can assume length of s1 <= length of s2.
    int64_t res = 0;
    int64_t i;
    for(i = 0; i < s1->len; i++){
        if (s1->contents[i] < s2->contents[i]){ res = 1; break; }
        if (s1->contents[i] > s2->contents[i]){ res = -1; break; }
    }
    // if res = 0 then the two strings agree upto length of s1; so, if s2 is longer it must be greater.
    if(res == 0 && s1->len < s2->len) res = 1;
    return res * flipped;
}

int64_t read_integer () {
    int64_t value;
    printf("Please enter an integer: ");
    scanf("%" PRId64 "" , &value);
    return value;
}

void print_integer (int64_t value) {
    printf("%" PRId64 "\n" , value);
}

// int main(int argsc, char **argsv){
//     struct string *tmp = NULL;
//     cmd_args = allocate_array(sizeof(struct string *), argsc, &tmp);
//     for(int i = 0; i < argsc; i++){
//         cmd_args[i] = raw_allocate_on_heap(sizeof(struct string));
//         cmd_args[i]->len = strlen(argsv[i]);
//         cmd_args[i]->contents = raw_allocate_on_heap(cmd_args[i]->len);
//         memcpy(cmd_args[i]->contents, argsv[i], cmd_args[i]->len);
//     }
//     return dolphin_fun_main();
// }