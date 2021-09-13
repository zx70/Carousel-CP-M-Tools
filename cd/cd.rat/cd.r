## cd - change directory
DRIVER(pwd)
 character buf(FILENAMESIZE)
 integer getarg
 integer cwdir

 call query ("usage: cd [directory].")
 if (getarg(1, buf, FILENAMESIZE) == EOF)
	call getdir (HOME_DIRECTORY, buf)
 if (cwdir (buf) == ERR)
	{
	call putlin (buf, ERROUT)
	call remark (": does not exist.")
	}
 DRETURN
 end
