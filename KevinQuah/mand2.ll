
declare i32 @printf(i8*, ...)

@.str_nl = private constant [2 x i8] c"\0A\00"
@.str_star = private constant [2 x i8] c"*\00"
@.str_space = private constant [2 x i8] c" \00"

define void @new_line() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_star() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_space() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @skip() #0 {
  ret void
}

@.str = private constant [3 x i8] c"%d\00"

define void @print_int(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.str, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
   ret void
}

@.char = private constant [3 x i8] c"%c\00"

define void @print_char(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.char, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}

; END OF BUILD-IN FUNCTIONS (prelude)

@Ymin = global double -1.3 

@Ymax = global double 1.3 

@Ystep = global double 0.05 

@Xmin = global double -2.1 

@Xmax = global double 1.1 

@Xstep = global double 0.02 

@Maxiters = global i32 1000 

define void @m_iter (i32 %m, double %x, double %y, double %zr, double %zi) {
   %tmp_1 = load i32, i32* @Maxiters
   %tmp_0 = icmp sle i32  %tmp_1, %m
   br i1 %tmp_0, label %if_branch_19, label %else_branch_20

if_branch_19:
   call void @print_char (i32 32)
   ret void

else_branch_20:
   %tmp_5 = fmul double  %zi, %zi
   %tmp_6 = fmul double  %zr, %zr
   %tmp_4 = fadd double  %tmp_5, %tmp_6
   %tmp_3 = fcmp ole double  4.0, %tmp_4
   br i1 %tmp_3, label %if_branch_21, label %else_branch_22

if_branch_21:
   %tmp_9 = srem i32  %m, 10
   %tmp_8 = add i32  48, %tmp_9
   call void @print_char (i32 %tmp_8)
   ret void

else_branch_22:
   %tmp_10 = add i32  %m, 1
   %tmp_13 = fmul double  %zr, %zr
   %tmp_14 = fmul double  %zi, %zi
   %tmp_12 = fsub double  %tmp_13, %tmp_14
   %tmp_11 = fadd double  %x, %tmp_12
   %tmp_17 = fmul double  %zr, %zi
   %tmp_16 = fmul double  2.0, %tmp_17
   %tmp_15 = fadd double  %tmp_16, %y
   call void @m_iter (i32 %tmp_10, double %x, double %y, double %tmp_11, double %tmp_15)
   ret void
}

define void @x_iter (double %x, double %y) {
   %tmp_24 = load double, double* @Xmax
   %tmp_23 = fcmp ole double  %x, %tmp_24
   br i1 %tmp_23, label %if_branch_29, label %else_branch_30

if_branch_29:
   call void @m_iter (i32 0, double %x, double %y, double 0.0, double 0.0)
   %tmp_27 = load double, double* @Xstep
   %tmp_26 = fadd double  %x, %tmp_27
   call void @x_iter (double %tmp_26, double %y)
   ret void

else_branch_30:
   call void @skip ()
   ret void
}

define void @y_iter (double %y) {
   %tmp_32 = load double, double* @Ymax
   %tmp_31 = fcmp ole double  %y, %tmp_32
   br i1 %tmp_31, label %if_branch_39, label %else_branch_40

if_branch_39:
   %tmp_33 = load double, double* @Xmin
   call void @x_iter (double %tmp_33, double %y)
   call void @print_char (i32 10)
   %tmp_37 = load double, double* @Ystep
   %tmp_36 = fadd double  %y, %tmp_37
   call void @y_iter (double %tmp_36)
   ret void

else_branch_40:
   call void @skip ()
   ret void
}

define i32 @main() {
   %tmp_41 = load double, double* @Ymin
   call void @y_iter (double %tmp_41)
   ret i32 0
}

