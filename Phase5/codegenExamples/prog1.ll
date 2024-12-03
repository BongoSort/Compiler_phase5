declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %y_loc_4 = alloca i64
 %x_loc_1 = alloca i64
 %tmp0 = add i64 5, 3
 store i64 %tmp0, i64* %x_loc_1
 %x2 = load i64, i64* %x_loc_1
 %tmp3 = add i64 %x2, 3
 store i64 %tmp3, i64* %y_loc_4
 %y5 = load i64, i64* %y_loc_4
 ret i64 %y5
}