declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x_loc_1 = alloca i64
 %call_0 = call i64 @read_integer ()
 store i64 %call_0, i64* %x_loc_1
 %x2 = load i64, i64* %x_loc_1
 %cmp3 = icmp sgt i64 %x2, 5
 br i1 %cmp3, label %then4, label %else6
then4:
 %x8 = load i64, i64* %x_loc_1
 ret i64 %x8
retdump_7:
 br label %after5
else6:
 %x10 = load i64, i64* %x_loc_1
 %tmp11 = sub i64 %x10, 10
 ret i64 %tmp11
retdump_9:
 br label %after5
after5:
 unreachable
}
