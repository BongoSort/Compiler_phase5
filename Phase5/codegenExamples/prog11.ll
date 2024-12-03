declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %cmp4 = icmp slt i64 3, 2
 br i1 %cmp4, label %tmp_1, label %eval_other_3
eval_other_3:
 %cmp5 = icmp slt i64 4, 6
 br label %shortor_0
tmp_1:
 br label %shortor_0
shortor_0:
 %res_2 = phi i1 [%cmp4, %tmp_1], [%cmp5, %eval_other_3]
 br i1 %res_2, label %then6, label %after7
then6:
 call void @print_integer (i64 4)
 br label %after7
after7:
 ret i64 0
}