%dlp_rec_Point = type { i64, i64 }
%dlp_rec_Elem = type { %dlp_rec_Point** }
%string_type = type { i64, i8* }
%dolphin_record_stream = type {  }

@str_lit_0 = global [7 x i8] c"\22hello\22"
@str_struct_1 = global { i64, [7 x i8]* } {i64 7, [7 x i8]* @str_lit_0}

declare void @print_integer(i64)
declare i64 @read_integer()
declare i8* @allocate_record(i32)
declare void @report_error_array_index_out_of_bounds()
declare i8* @allocate_array(i32, i64, i8*)
declare void @output_string(%string_type*, %dolphin_record_stream*)
declare %dolphin_record_stream* @get_stdout()

define i64 @main () {
 %x_loc_60 = alloca %string_type*
 %z_loc_58 = alloca %dolphin_record_stream*
 %ar_loc_19 = alloca %dlp_rec_Point**
 %init_value_ptr_15 = alloca i8
 %p_loc_13 = alloca %dlp_rec_Point*
 %size_ptr_7 = getelementptr %dlp_rec_Point, %dlp_rec_Point* null, i32 1
 %size_8 = ptrtoint %dlp_rec_Point* %size_ptr_7 to i32
 %ptr_rt9 = call i8* @allocate_record (i32 %size_8)
 %Point_10 = bitcast i8* %ptr_rt9 to %dlp_rec_Point*
 %ptr_field_x11 = getelementptr %dlp_rec_Point, %dlp_rec_Point* %Point_10, i64 0, i32 0
 store i64 5, i64* %ptr_field_x11
 %ptr_field_y12 = getelementptr %dlp_rec_Point, %dlp_rec_Point* %Point_10, i64 0, i32 1
 store i64 10, i64* %ptr_field_y12
 store %dlp_rec_Point* %Point_10, %dlp_rec_Point** %p_loc_13
 store i8 0, i8* %init_value_ptr_15
 %array_gep_16 = getelementptr %dlp_rec_Point**, %dlp_rec_Point*** null, i32 1
 %elem_size_17 = ptrtoint %dlp_rec_Point*** %array_gep_16 to i32
 %array_ptr_14 = call i8* @allocate_array (i32 %elem_size_17, i64 5, i8* %init_value_ptr_15)
 %array_res_18 = bitcast i8* %array_ptr_14 to %dlp_rec_Point**
 store %dlp_rec_Point** %array_res_18, %dlp_rec_Point*** %ar_loc_19
 %ar20 = load %dlp_rec_Point**, %dlp_rec_Point*** %ar_loc_19
 ; Get the i64 array pointer from array_ptr
 %i64_arr_ptr_22 = bitcast %dlp_rec_Point** %ar20 to i64*
 %array_ptr_21 = call i64 @dolphin_rc_get_array_length (i64* %i64_arr_ptr_22)
 %cmp_25 = icmp sle i64 0, %array_ptr_21
 br i1 %cmp_25, label %idx_ok_23, label %idx_fail_24
idx_fail_24:
 call void @report_error_array_index_out_of_bounds ()
 br label %idx_ok_23
idx_ok_23:
 %elem_ptr26 = getelementptr %dlp_rec_Point*, %dlp_rec_Point** %ar20, i64 0
 %p27 = load %dlp_rec_Point*, %dlp_rec_Point** %p_loc_13
 store %dlp_rec_Point* %p27, %dlp_rec_Point** %elem_ptr26
 %ar28 = load %dlp_rec_Point**, %dlp_rec_Point*** %ar_loc_19
 %generic_arr_ptr_29 = bitcast %dlp_rec_Point** %ar28 to i64*
 %arr_size_30 = call i64 @dolphin_rc_get_array_length (i64* %generic_arr_ptr_29)
 %cmp_33 = icmp sle i64 0, %arr_size_30
 br i1 %cmp_33, label %idx_ok_31, label %idx_fail_32
idx_fail_32:
 call void @report_error_array_index_out_of_bounds ()
 br label %idx_ok_31
idx_ok_31:
 %elem_ptr34 = getelementptr %dlp_rec_Point*, %dlp_rec_Point** %ar28, i64 0
 %elem_35 = load %dlp_rec_Point*, %dlp_rec_Point** %elem_ptr34
 %elem_ptr_36 = getelementptr %dlp_rec_Point, %dlp_rec_Point* %elem_35, i64 0, i32 0
 %elem_val_37 = load i64, i64* %elem_ptr_36
 call void @print_integer (i64 %elem_val_37)
 ; Field assignment.
 %ar39 = load %dlp_rec_Point**, %dlp_rec_Point*** %ar_loc_19
 %generic_arr_ptr_40 = bitcast %dlp_rec_Point** %ar39 to i64*
 %arr_size_41 = call i64 @dolphin_rc_get_array_length (i64* %generic_arr_ptr_40)
 %cmp_44 = icmp sle i64 0, %arr_size_41
 br i1 %cmp_44, label %idx_ok_42, label %idx_fail_43
idx_fail_43:
 call void @report_error_array_index_out_of_bounds ()
 br label %idx_ok_42
idx_ok_42:
 %elem_ptr45 = getelementptr %dlp_rec_Point*, %dlp_rec_Point** %ar39, i64 0
 %elem_46 = load %dlp_rec_Point*, %dlp_rec_Point** %elem_ptr45
 ; Get the field pointer
 %field_ptr_38 = getelementptr %dlp_rec_Point, %dlp_rec_Point* %elem_46, i64 0, i32 0
 store i64 50, i64* %field_ptr_38
 %ar47 = load %dlp_rec_Point**, %dlp_rec_Point*** %ar_loc_19
 %generic_arr_ptr_48 = bitcast %dlp_rec_Point** %ar47 to i64*
 %arr_size_49 = call i64 @dolphin_rc_get_array_length (i64* %generic_arr_ptr_48)
 %cmp_52 = icmp sle i64 0, %arr_size_49
 br i1 %cmp_52, label %idx_ok_50, label %idx_fail_51
idx_fail_51:
 call void @report_error_array_index_out_of_bounds ()
 br label %idx_ok_50
idx_ok_50:
 %elem_ptr53 = getelementptr %dlp_rec_Point*, %dlp_rec_Point** %ar47, i64 0
 %elem_54 = load %dlp_rec_Point*, %dlp_rec_Point** %elem_ptr53
 %elem_ptr_55 = getelementptr %dlp_rec_Point, %dlp_rec_Point* %elem_54, i64 0, i32 0
 %elem_val_56 = load i64, i64* %elem_ptr_55
 call void @print_integer (i64 %elem_val_56)
 %call_57 = call %dolphin_record_stream* @get_stdout ()
 store %dolphin_record_stream* %call_57, %dolphin_record_stream** %z_loc_58
 ; String literal
 %str_cast_59 = bitcast { i64, [7 x i8]* }* @str_struct_1 to %string_type*
 store %string_type* %str_cast_59, %string_type** %x_loc_60
 %z61 = load %dolphin_record_stream*, %dolphin_record_stream** %z_loc_58
 %x62 = load %string_type*, %string_type** %x_loc_60
 call void @output_string (%string_type* %x62, %dolphin_record_stream* %z61)
 ret i64 0
retdump_63:
 unreachable
}

define i64 @dolphin_rc_get_array_length (i64* %array) {
 %len_ptr_2 = getelementptr i64, i64* %array, i64 -1
 %size_3 = load i64, i64* %len_ptr_2
 ret i64 %size_3
}

define void @dolphin_rc_set_array_length (i64* %array, i64 %size) {
 %len_ptr_4 = getelementptr i64, i64* %array, i64 -1
 store i64 %size, i64* %len_ptr_4
 ret void
}

define i32 @dolphin_rc_compute_array_length_size () {
 %size_ptr_5 = getelementptr i64, i64* null, i64 1
 %size_6 = ptrtoint i64* %size_ptr_5 to i32
 ret i32 %size_6
}
