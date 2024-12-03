declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @even (i64 %x) {
 %arg_13 = alloca i64
 store i64 %x, i64* %arg_13
 %x14 = load i64, i64* %arg_13
 %cmp15 = icmp eq i64 %x14, 0
 br i1 %cmp15, label %then16, label %else18
then16:
 ret i64 0
retdump_19:
 br label %after17
else18:
 %x21 = load i64, i64* %arg_13
 %tmp22 = sub i64 %x21, 1
 %call_23 = call i64 @odd (i64 %tmp22)
 ret i64 %call_23
retdump_20:
 br label %after17
after17:
 unreachable
}

define i64 @odd (i64 %x) {
 %arg_2 = alloca i64
 store i64 %x, i64* %arg_2
 %x3 = load i64, i64* %arg_2
 %cmp4 = icmp eq i64 %x3, 0
 br i1 %cmp4, label %then5, label %else7
then5:
 ret i64 1
retdump_8:
 br label %after6
else7:
 %x10 = load i64, i64* %arg_2
 %tmp11 = sub i64 %x10, 1
 %call_12 = call i64 @even (i64 %tmp11)
 ret i64 %call_12
retdump_9:
 br label %after6
after6:
 unreachable
}

define i64 @main () {
 %call_1 = call i64 @even (i64 5)
 ret i64 %call_1
retdump_0:
 unreachable
}
