declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x_loc_2 = alloca i64
 %y_loc_1 = alloca i1
 %cmp0 = icmp slt i64 3, 2
 store i1 %cmp0, i1* %y_loc_1
 store i64 3, i64* %x_loc_2
 ret i64 5
}
