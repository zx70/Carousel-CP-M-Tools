 #   include the standard symbol definitions
 
 ## cat - concatenate named files onto standard output
 DRIVER
 
 character buf(MAXLINE)
 integer getarg, open
 integer i, int
 
 for (i=1; getarg(i, buf, MAXLINE) != EOF; i=i+1)
            {
            if (buf(1) == MINUS & buf(2) == EOS)
                int = STDIN
            else if (buf(1) == QMARK & buf(2) == EOS)
                call error ('usage:  cat [file ] .')
            else
                int = open(buf, READ)
            if (int == ERR)
                call cant(buf)
            call fcopy(int, STDOUT)
            if (int != STDIN)
                call close(int)
            }
        if (i == 1)     # no arguments passed
                call fcopy (STDIN, STDOUT)
        DRETURN
        end
