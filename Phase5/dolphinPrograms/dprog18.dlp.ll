declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @myFunction () {
 ret i64 5
retdump_3:
 unreachable
}

define i64 @main () {
 %call_0 = call i64 @myFunction ()
 call void @print_integer (i64 %call_0)
 %call_2 = call i64 @myFunction ()
 ret i64 %call_2
retdump_1:
 unreachable
}
