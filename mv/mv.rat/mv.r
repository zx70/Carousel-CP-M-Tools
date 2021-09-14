# mv - move file1 to file2, copying if necessary.
 DRIVER
   character file1(MAXNAME), file2(MAXNAME)
   integer amove, open, create, getarg

   if (getarg(1, file1, MAXNAME) == EOF |
       getarg(2, file2, MAXNAME) == EOF)
          call error("usage: mv file1 file2.")
   call gdest (file1, file2)
   if (amove(file1, file2) == ERR)
	{	# cant move
	call remark("can't move.")
	}
 DRETURN
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
