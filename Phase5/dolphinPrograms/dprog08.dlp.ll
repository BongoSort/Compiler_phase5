declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %cmp0 = icmp slt i64 5, 6
 br i1 %cmp0, label %then1, label %after2
then1:
 ret i64 3
retdump_3:
 br label %after2
after2:
 ret i64 0
retdump_4:
 unreachable
}
