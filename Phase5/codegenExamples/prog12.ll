declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %cmp4 = icmp slt i64 3, 2
 br i1 %cmp4, label %eval3, label %tmp1
eval3:
 %cmp5 = icmp slt i64 4, 6
 br label %shortand_0
tmp1:
 br label %shortand_0
shortand_0:
 %res2 = phi i1 [%cmp4, %tmp1], [%cmp5, %eval3]
 br i1 %res2, label %then6, label %after7
then6:
 call void @print_integer (i64 4)
 br label %after7
after7:
 ret i64 0
}