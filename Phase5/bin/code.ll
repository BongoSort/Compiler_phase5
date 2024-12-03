declare void @print_integer(i64)     
declare i64 @read_integer()

define i64 @dolphin_main () {
 %cmp0 = icmp slt i64 3, 2
 br i1 %cmp0, label %then1, label %after2
then1:
 %cmp3 = icmp slt i64 3, 1
 br i1 %cmp3, label %then4, label %else6
then4:
 ret i64 5
else6:
 ret i64 2
after5:
 ret i64 0
after2:
 %cmp7 = icmp slt i64 3, 2
 br i1 %cmp7, label %then8, label %else10
then8:
 ret i64 1
else10:
 ret i64 9
after9:
 ret i64 0
}