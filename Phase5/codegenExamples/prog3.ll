declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x_loc_6 = alloca i64
 %x_loc_5 = alloca i64
 %x_loc_0 = alloca i64
 store i64 5, i64* %x_loc_0
 %cmp1 = icmp eq i64 1, 1
 br i1 %cmp1, label %then2, label %else4
then2:
 store i64 10, i64* %x_loc_5
 br label %after3
else4:
 store i64 20, i64* %x_loc_6
 br label %after3
after3:
 %x7 = load i64, i64* %x_loc_0
 ret i64 %x7
}