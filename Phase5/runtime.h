#include <stdint.h>

struct string {int64_t len; char * contents; };
extern struct string *dolphin_rc_empty_string;
void *raw_allocate_on_heap(int32_t size);
void *allocate_record (int32_t size);
void *allocate_array(int32_t size, int64_t numelems, void* contents);
void report_error_division_by_zero();
void report_error_nil_access();
void report_error_array_index_out_of_bounds();
int64_t string_length (struct string*);