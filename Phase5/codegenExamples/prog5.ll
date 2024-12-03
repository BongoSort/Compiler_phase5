declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %tmp0 = sdiv i64 10, 2
 ret i64 %tmp0
}