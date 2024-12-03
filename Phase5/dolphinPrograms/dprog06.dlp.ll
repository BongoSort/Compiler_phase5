declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %tmp1 = sdiv i64 10, 2
 ret i64 %tmp1
retdump_0:
 unreachable
}
