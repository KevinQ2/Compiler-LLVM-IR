
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
   br i1 %tmp_0, label %if_branch_17, label %else_branch_18

if_branch_17:
   call void @print_star ()
   ret void

else_branch_18:
   %tmp_5 = fmul double  %zi, %zi
   %tmp_6 = fmul double  %zr, %zr
   %tmp_4 = fadd double  %tmp_5, %tmp_6
   %tmp_3 = fcmp ole double  4.0, %tmp_4
   br i1 %tmp_3, label %if_branch_19, label %else_branch_20

if_branch_19:
   call void @print_space ()
   ret void

else_branch_20:
   %tmp_8 = add i32  %m, 1
   %tmp_11 = fmul double  %zr, %zr
   %tmp_12 = fmul double  %zi, %zi
   %tmp_10 = fsub double  %tmp_11, %tmp_12
   %tmp_9 = fadd double  %x, %tmp_10
   %tmp_15 = fmul double  %zr, %zi
   %tmp_14 = fmul double  2.0, %tmp_15
   %tmp_13 = fadd double  %tmp_14, %y
   call void @m_iter (i32 %tmp_8, double %x, double %y, double %tmp_9, double %tmp_13)
   ret void
}

define void @x_iter (double %x, double %y) {
   %tmp_22 = load double, double* @Xmax
   %tmp_21 = fcmp ole double  %x, %tmp_22
   br i1 %tmp_21, label %if_branch_27, label %else_branch_28

if_branch_27:
   call void @m_iter (i32 0, double %x, double %y, double 0.0, double 0.0)
   %tmp_25 = load double, double* @Xstep
   %tmp_24 = fadd double  %x, %tmp_25
   call void @x_iter (double %tmp_24, double %y)
   ret void

else_branch_28:
   call void @skip ()
   ret void
}

define void @y_iter (double %y) {
   %tmp_30 = load double, double* @Ymax
   %tmp_29 = fcmp ole double  %y, %tmp_30
   br i1 %tmp_29, label %if_branch_37, label %else_branch_38

if_branch_37:
   %tmp_31 = load double, double* @Xmin
   call void @x_iter (double %tmp_31, double %y)
   call void @new_line ()
   %tmp_35 = load double, double* @Ystep
   %tmp_34 = fadd double  %y, %tmp_35
   call void @y_iter (double %tmp_34)
   ret void

else_branch_38:
   call void @skip ()
   ret void
}

define i32 @main() {
   %tmp_39 = load double, double* @Ymin
   call void @y_iter (double %tmp_39)
   ret i32 0
}

