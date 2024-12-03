declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %cmp0 = icmp slt i64 5, 6
 br i1 %cmp0, label %then1, label %after2
then1:
 ret i64 3
after2:
 ret i64 0
}