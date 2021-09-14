 ## which - report full path of command
 DRIVER
 
 character buf(MAXLINE)
 integer getarg, getlin
 integer i, j
 
 for (i=1; getarg(i, buf, MAXLINE) != EOF; i=i+1)
            {
            if (buf(1) == MINUS & buf(2) == EOS)
		{
		repeat
			{
			j = getlin(buf,STDIN)
			if (j == EOF)
				break
			buf(j) = EOS
			call which (buf)
			}
		}
            else if (buf(1) == QMARK & buf(2) == EOS)
                call error ('usage:  which [file ] .')
            else
		call which (buf)
            }
        DRETURN
        end
