declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %z_loc_15 = alloca i64
 %y_loc_14 = alloca i64
 %x_loc_13 = alloca i1
 %x_loc_2 = alloca i64
 %y_loc_1 = alloca i64
 %tmp0 = sub i64 0, 9223372036854775807
 store i64 %tmp0, i64* %y_loc_1
 store i64 3, i64* %x_loc_2
 br label %for_init_3
for_init_3:
 store i64 0, i64* %x_loc_2
 br label %for_cond_4
for_cond_4:
 %x8 = load i64, i64* %x_loc_2
 %cmp9 = icmp slt i64 %x8, 5
 br i1 %cmp9, label %for_body_6, label %for_after_7
for_body_6:
 %x10 = load i64, i64* %x_loc_2
 call void @print_integer (i64 %x10)
 br label %for_update_5
for_update_5:
 %x11 = load i64, i64* %x_loc_2
 %tmp12 = add i64 %x11, 1
 store i64 %tmp12, i64* %x_loc_2
 br label %for_cond_4
for_after_7:
 store i1 0, i1* %x_loc_13
 store i64 5, i64* %y_loc_14
 store i64 3, i64* %z_loc_15
 %x16 = load i1, i1* %x_loc_13
 br i1 %x16, label %then17, label %after18
then17:
 %x19 = load i1, i1* %x_loc_13
 %bool20 = xor i1 %x19, 1
 br i1 %bool20, label %then21, label %else23
then21:
 store i64 1, i64* %z_loc_15
 br label %after22
else23:
 store i64 2, i64* %z_loc_15
 br label %after22
after22:
 br label %after18
after18:
 store i64 5, i64* %z_loc_15
 %z25 = load i64, i64* %z_loc_15
 ret i64 %z25
retdump_24:
 unreachable
}
