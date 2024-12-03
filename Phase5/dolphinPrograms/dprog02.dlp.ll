declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %y_loc_3 = alloca i64
 %x_loc_1 = alloca i64
 %tmp0 = add i64 5, 3
 store i64 %tmp0, i64* %x_loc_1
 %tmp2 = add i64 7, 3
 store i64 %tmp2, i64* %y_loc_3
 %y5 = load i64, i64* %y_loc_3
 ret i64 %y5
retdump_4:
 unreachable
}
