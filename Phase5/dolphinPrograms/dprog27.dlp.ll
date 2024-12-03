declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x_loc_0 = alloca i64
 store i64 5, i64* %x_loc_0
 %x1 = load i64, i64* %x_loc_0
 call void @print_integer (i64 %x1)
 store i64 10, i64* %x_loc_0
 %x2 = load i64, i64* %x_loc_0
 call void @print_integer (i64 %x2)
 ret i64 1
retdump_3:
 unreachable
}
