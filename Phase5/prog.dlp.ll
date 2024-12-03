declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %i_loc_20 = alloca i64
 br i1 1, label %then8, label %else10
then8:
 br i1 0, label %then11, label %else13
then11:
 ret i64 2
retdump_14:
 br label %after12
else13:
 br label %for_init_15
for_init_15:
 store i64 0, i64* %i_loc_20
 br label %for_cond_16
for_cond_16:
 %i21 = load i64, i64* %i_loc_20
 %cmp22 = icmp slt i64 %i21, 5
 br i1 %cmp22, label %for_body_18, label %for_after_19
for_body_18:
 %i23 = load i64, i64* %i_loc_20
 call void @test (i64 %i23)
 br label %for_update_17
for_update_17:
 %i24 = load i64, i64* %i_loc_20
 %tmp25 = add i64 %i24, 1
 store i64 %tmp25, i64* %i_loc_20
 br label %for_cond_16
for_after_19:
 call void @testing (i64 2)
 ret i64 4
retdump_26:
 br label %after12
after12:
 br label %after9
else10:
 call void @test (i64 3)
 ret i64 0
retdump_27:
 br label %after9
after9:
 unreachable
}

define void @test (i64 %i) {
 %arg_6 = alloca i64
 store i64 %i, i64* %arg_6
 %i7 = load i64, i64* %arg_6
 call void @print_integer (i64 %i7)
 ret void
}

define void @testing (i64 %i) {
 %arg_0 = alloca i64
 store i64 %i, i64* %arg_0
 %i1 = load i64, i64* %arg_0
 %cmp2 = icmp sgt i64 %i1, 5
 br i1 %cmp2, label %then3, label %after4
then3:
 %i5 = load i64, i64* %arg_0
 call void @print_integer (i64 %i5)
 br label %after4
after4:
 call void @print_integer (i64 10)
 ret void
}
