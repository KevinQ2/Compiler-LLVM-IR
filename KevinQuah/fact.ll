
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

define i32 @fact (i32 %n) {
   %tmp_0 = icmp eq i32  %n, 0
   br i1 %tmp_0, label %if_branch_4, label %else_branch_5

if_branch_4:
   ret i32 1

else_branch_5:
   %tmp_2 = sub i32  %n, 1
   %tmp_3 = call i32 @fact (i32 %tmp_2)
   %tmp_1 = mul i32  %n, %tmp_3
   ret i32 %tmp_1
}

define i32 @facT (i32 %n, i32 %acc) {
   %tmp_6 = icmp eq i32  %n, 0
   br i1 %tmp_6, label %if_branch_10, label %else_branch_11

if_branch_10:
   ret i32 %acc

else_branch_11:
   %tmp_7 = sub i32  %n, 1
   %tmp_8 = mul i32  %n, %acc
   %tmp_9 = call i32 @facT (i32 %tmp_7, i32 %tmp_8)
   ret i32 %tmp_9
}

define i32 @facTi (i32 %n) {
   %tmp_12 = call i32 @facT (i32 %n, i32 1)
   ret i32 %tmp_12
}

define void @top () {
   %tmp_14 = call i32 @fact (i32 6)
   call void @print_int (i32 %tmp_14)
   call void @print_char (i32 44)
   %tmp_17 = call i32 @facTi (i32 6)
   call void @print_int (i32 %tmp_17)
   call void @print_char (i32 10)
   ret void
}

define i32 @main() {
   call void @top ()
   ret i32 0
}

