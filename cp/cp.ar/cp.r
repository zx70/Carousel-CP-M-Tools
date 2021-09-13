## cp - copy file1 to file2 (binary read/write)
DRIVER(cp)
 
integer getarg, open, isatty, create, equal, length, index, isopen
integer fd1, fd2
character file1(FILENAMESIZE), file2(FILENAMESIZE), temp(FILENAMESIZE)
integer len, i, j
string dot "."

 call query ("usage:  cp from to.")
 if (getarg(1, file1, FILENAMESIZE) != EOF)
	{
	fd1 = open (file1, READ)
	if (fd1 == ERR)
		call cant (file1)
	}
 if (getarg(2, file2, FILENAMESIZE) != EOF)
	{
	call gdest (file1, file2)
	if (isopen (file2) != ERR)
		call error("cp won't copy infile onto itself.")
	fd2 = create (file2, WRITE)
	if (fd2 == ERR)
		call cant (file2)
	}
 else
	call error ("usage:  cp from to.")
 if (isatty(fd1) == YES | isatty(fd2) == YES)
	call error ("use 'cat' for terminal I/O.")
 call bcopy$ (fd1, fd2)
 call close (fd1)
 call close (fd2)
return
end

 subroutine gdest (src, dest)
 character src(ARB), dest(ARB)
 integer equal, ctoc, index
 character temp (FILENAMESIZE)
 integer i, j
 string dot "."

 if (equal(dest, dot) == YES)
	call gwdir (temp, LOCAL)
 else
	call mklocl(dest,temp)
 i = ctoc (temp, dest, FILENAMESIZE)
 if (dest(i) == COLON)
	{
	call mklocl (src, temp)
	j = index (temp, COLON) + 1
	call ctoc (temp(j), dest(i+1), FILENAMESIZE)
	}
 return
 end
