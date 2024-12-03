declare void @print_integer(i64)
declare i64 @read_integer()

define i1 @myFunction () {
 ret i1 1
retdump_3:
 unreachable
}

define i64 @main () {
 %x_loc_1 = alloca i1
 %call_0 = call i1 @myFunction ()
 store i1 %call_0, i1* %x_loc_1
 ret i64 0
retdump_2:
 unreachable
}
