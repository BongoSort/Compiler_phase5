declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x_loc_0 = alloca i64
 store i64 1, i64* %x_loc_0
 %x2 = load i64, i64* %x_loc_0
 %tmp3 = add i64 %x2, 1
 store i64 %tmp3, i64* %x_loc_0
 ret i64 1
retdump_1:
 unreachable
}
