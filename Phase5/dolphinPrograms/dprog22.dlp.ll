declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @myFunction (i64 %x, i1 %y) {
 %arg_3 = alloca i64
 %arg_2 = alloca i1
 store i1 %y, i1* %arg_2
 store i64 %x, i64* %arg_3
 %y4 = load i1, i1* %arg_2
 br i1 %y4, label %then5, label %after6
then5:
 %x7 = load i64, i64* %arg_3
 call void @print_integer (i64 %x7)
 %x9 = load i64, i64* %arg_3
 ret i64 %x9
retdump_8:
 br label %after6
after6:
 ret i64 111
retdump_10:
 unreachable
}

define i64 @main () {
 %call_0 = call i64 @myFunction (i64 5, i1 1)
 ret i64 0
retdump_1:
 unreachable
}
