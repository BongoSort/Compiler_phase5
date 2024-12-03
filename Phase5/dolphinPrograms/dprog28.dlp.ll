declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %tmp0 = add i64 3, 1
 %tmp1 = sub i64 2, 1
 br i1 1, label %then2, label %after3
then2:
 call void @print_integer (i64 4)
 br label %after3
after3:
 ret i64 1
retdump_4:
 unreachable
}
