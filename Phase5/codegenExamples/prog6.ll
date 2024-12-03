declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %y_loc_3 = alloca i1
 %x_loc_0 = alloca i1
 store i1 1, i1* %x_loc_0
 %x1 = load i1, i1* %x_loc_0
 %bool2 = xor i1 %x1, 1
 store i1 %bool2, i1* %y_loc_3
 ret i64 0
}