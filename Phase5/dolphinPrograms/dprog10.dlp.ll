declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x_loc_1 = alloca i64
 %x_loc_0 = alloca i64
 store i64 5, i64* %x_loc_0
 store i64 10, i64* %x_loc_1
 %x3 = load i64, i64* %x_loc_1
 ret i64 %x3
retdump_2:
 unreachable
}