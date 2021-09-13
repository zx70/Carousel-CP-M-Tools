 ## definitions for kwic and unrot tools
 
 define(FOLD,DOLLAR)    #character to indicate beginning of folded line
 define(MAXOUT,80)      #width of index
 #---------------------------------------------------------------------
 
 ## kwic - make keyword in context index
 DRIVER(kwic)
 
 character buf(MAXLINE)
 integer getarg, open, getlin
 integer i, int
 
 call query ("usage:  kwic [file].")
 for (i=1; getarg(i,buf,MAXLINE)!=EOF; i=i+1)
        {
        if (buf(1) == MINUS & buf(2) == EOS)
                int = STDIN
        else
                {
                int = open(buf,READ)
                if (int == ERR)
                        call cant(buf)
                }
        while(getlin(buf,int) != EOF)
                call putrot(buf, STDOUT)
        if (int != STDIN)
                call close(int)
        }
 
 if (i==1)              #Read from standard input
        while (getlin(buf,STDIN) != EOF)
                call putrot(buf, STDOUT)
 DRETURN
 end
 ## putrot - create lines with keyword at front
 subroutine putrot (buf, outfil)
 
 character type
 character buf(ARB), t
 integer i, outfil
 
 for (i=1; buf(i) != NEWLINE; i=i+1)
        {
        t = type(buf(i))
        if (t == LETTER | t == DIGIT)   #alpha
                {
                call rotate(buf, i, outfil)     #token starts at 'i'
                t = type(buf(i+1))
                for (; t==LETTER | t==DIGIT; t=type(buf(i+1)))
                        i = i + 1
                }
        }
 
 return
 end
 ## rotate - output rotated line
 subroutine rotate(buf, n, outfil)
 
 character buf(ARB)
 integer i, n, outfil
 
 for (i=n; buf(i) != NEWLINE; i=i+1)
        call putch(buf(i), outfil)
 call putch(FOLD, outfil)
 for (i=1; i<n; i=i+1)
        call putch(buf(i), outfil)
 call putch(NEWLINE, outfil)
 return
 end
