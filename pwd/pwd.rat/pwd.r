## pwd - print name of working (current) directory
DRIVER(pwd)
 character buf(3)
 integer dtype
 integer getarg

 call query ("usage: pwd [-l].")
 dtype = PATH
 if (getarg(1, buf, 3) != EOF)
	if (buf(1) == MINUS & (buf(2) == LETL | buf(2) == BIGL) )
		dtype = LOCAL
 call gwdir (buf, dtype)
 call putlin (buf, STDOUT)
 call putch (NEWLINE, STDOUT)
 return
 end
