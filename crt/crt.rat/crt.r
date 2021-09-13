##crt - prepare output for teletype-like device
 
 DRIVER
 
 
 integer getarg, ctoi, open, getlin, crt
 integer i, j, tt, nlines, input
 character buf(MAXLINE), terml(FILENAMESIZE)
 string tty TERMINAL_IN
 data nlines /23/
 data input /NO/
 
 
 
 call query ("usage:  crt [-n] [file].")
 tt = open(tty, READ)
 if (tt == ERR)
        call cant(tty)
 for (i=1; getarg(i,buf,MAXLINE)!=EOF; i=i+1)
        {
        if (buf(1) == MINUS & buf(2) == EOS)
                int = STDIN
        else if (buf(1) == MINUS)
                {
                j = 2
                nlines = max(ctoi(buf,j), 1)
                next
                }
        else
                {
                int = open(buf,READ)
                if (int == ERR)
                        call cant(buf)
                }
 
        input = YES
        if (crt(int, nlines, tt) == EOF)
                break
         if (int != STDIN)
                call close(int)
        }
 
 if (input == NO)
        call crt(STDIN, nlines, tt)
 return
 end
## crt - look at file "fd", stopping after each nl lines
integer function crt (fd, nl, tt)
 integer fd, nl, wait, tt
 integer getlin, isatty, prompt
 character buf(MAXLINE), what(MAXLINE)
 string pr "Hit <CR> to continue, q to quit: "
 
 crt = OK
 if (getlin(buf,fd) == EOF)
	return
 j = 1
 repeat
        {
	call putlin (buf, STDOUT)
	if (getlin (buf, fd) == EOF)
		return
	j = j+1
	if (j > nl)
		{
		if (isatty (STDOUT) == YES)
			{
			if (prompt (pr, what, tt) == EOF)
				return(EOF)
			if (what(1) == LETQ | what(1) == BIGQ)
				return
			}
		j = 1
		}
        }
 return
 end
