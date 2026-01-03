@.str = private constant [13 x i8] c"hello, world\00"

declare i32 @puts(i8*)

define i32 @main()
{
  %ptr = getelementptr [13 x i8], [13 x i8]* @.str, i32 0, i32 0
  call i32 (i8*) @puts(i8* %ptr)
  ret i32 0
}
