declare void @print_integer(i64)
declare i64 @read_integer()

define void @myFunction () {
 br i1 1, label %then1, label %after2
then1:
 call void @print_integer (i64 5)
 ret void
retdump_3:
 br label %after2
after2:
 call void @print_integer (i64 7)
 ret void
}

define i64 @main () {
 call void @myFunction ()
 ret i64 0
retdump_0:
 unreachable
}
