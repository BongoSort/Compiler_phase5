declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x_loc_1 = alloca i64
 %call_0 = call i64 @read_integer ()
 store i64 %call_0, i64* %x_loc_1
 %tmp2 = add i64 3, 2
 %x3 = load i64, i64* %x_loc_1
 %cmp4 = icmp sgt i64 %x3, 5
 br i1 %cmp4, label %then5, label %else7
then5:
 %x8 = load i64, i64* %x_loc_1
 call void @print_integer (i64 %x8)
 ret i64 1
retdump_9:
 br label %after6
else7:
 %x10 = load i64, i64* %x_loc_1
 %tmp11 = add i64 %x10, 10
 call void @print_integer (i64 %tmp11)
 ret i64 0
retdump_12:
 br label %after6
after6:
 unreachable
}
