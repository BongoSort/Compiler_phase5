declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %tmp0 = sub i64 3, 10
 ret i64 %tmp0
}
