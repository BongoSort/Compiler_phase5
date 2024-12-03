%test0 = type { i64, i1, i1, i1 }

declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 call void @print_integer (i64 3)
 ret i64 1
retdump_1:
 unreachable
}
