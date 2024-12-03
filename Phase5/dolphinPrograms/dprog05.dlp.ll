declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x_loc_5 = alloca i64
 %x_loc_0 = alloca i64
 store i64 5, i64* %x_loc_0
 %cmp1 = icmp slt i64 2, 4
 br i1 %cmp1, label %then2, label %else4
then2:
 store i64 10, i64* %x_loc_5
 %x7 = load i64, i64* %x_loc_5
 ret i64 %x7
retdump_6:
 br label %after3
else4:
 %x9 = load i64, i64* %x_loc_0
 ret i64 %x9
retdump_8:
 br label %after3
after3:
 ret i64 0
retdump_10:
 unreachable
}
