 ## definitions for overstrike tool
 # put on a file named 'oversym'
 # Used only by overstrike
 
 define(NOSKIP,PLUS)            #suppress carriage-return/line feed
 define(SKIP,BLANK)
 #----------------------------------------------------------------------
 ## os - convert backspaces into multiple lines
 DRIVER(os)
 
 character buf(MAXLINE)
 integer getarg, open
 integer i, fd
 
 call query ("usage:  os [file].")
 for (i=1; getarg(i, buf, MAXLINE) != EOF; i=i+1)
        {
        if (buf(1) == MINUS & buf(2) == EOS)
                fd = STDIN
        else
                {
                fd = open(buf,READ)
                if (fd == ERR)
                        call cant(buf)
                }
        call overs (fd)
        if (fd != STDIN)
                call close(fd)
        }
 if (i == 1)
        call overs (STDIN)
 
 DRETURN
 end
 ## overs - convert backspaces into multiple lines from file -int-
 subroutine overs(int)
 character getch
 character c
 integer col, newcol, int
 
 col = 1
 repeat
        {
        newcol = col
        while (getch(c,int) == BACKSPACE)       #eat up backspaces
                newcol = max(newcol-1, 1)
        if (newcol < col)                       #start overstrike line
                {
                call putc(NEWLINE)
                call putc(NOSKIP)
                for (col=1; col<newcol; col=col+1)
                        call putc(BLANK)
                }
        else if (col == 1 & c != EOF)           #start normal line
                call putc(SKIP)
                                                #else middle of line
        if (c == EOF)
                break
        call putc(c)                            #normal character
        if (c == NEWLINE)
                col = 1
        else
                col = col + 1
        }
 return
 end
