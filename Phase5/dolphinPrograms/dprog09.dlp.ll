declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %y_loc_6 = alloca i64
 %x_loc_4 = alloca i64
 %x_loc_0 = alloca i64
 store i64 5, i64* %x_loc_0
 %cmp1 = icmp eq i64 1, 1
 br i1 %cmp1, label %then2, label %after3
then2:
 store i64 20, i64* %x_loc_4
 %x5 = load i64, i64* %x_loc_4
 store i64 %x5, i64* %y_loc_6
 br label %after3
after3:
 %x8 = load i64, i64* %x_loc_0
 ret i64 %x8
retdump_7:
 unreachable
}
