 define(DEFAULT_WIDTH,25)
 define(DEFAULT_DIF,8)
 define(LEFT,0)
 define(RIGHT,1)
 define(DEFAULT_JUSTFY,LEFT)
 define(NULLPOINTER,-1)		#????
 DRIVER(isam)

 integer n, status, i, junk
 integer getlin, getwrd, doline, ptreq
 character oldwrd(MAXLINE), buf(MAXLINE), word(MAXLINE)

 include cisam

 call query("usage:  isam [-d<dif>] [-w<width>] [-j<l/r>].")
 call getcmd                    # crack command line
 oldwrd(1) = EOS
 call ptrcpy(NULLPOINTER, oldadr)
 n = 0
 repeat
    {
    call note(addr, STDIN)
    status = getlin(buf, STDIN)
    if (status != EOF)
        {
        n = n + 1
        i = 1
        junk = getwrd(buf, i, word)
        if (doline(n) == YES)
            call outlin(word, addr)
        call ctoc(word, oldwrd, MAXLINE)
        call ptrcpy(addr, oldadr)
        }
    }
 until (status == EOF)
 if (doline(n) == NO & ptreq(oldadr, NULLPOINTER) == NO)
    call outlin(oldwrd, oldadr)

 DRETURN
 end

 integer function doline(n)
 integer n
 integer b
 include cisam
 b = addr(2)
 if (b != oldadr(2) & mod(b,dif) == 0)
	return(YES)
 return(NO)
 end

 subroutine getcmd

 character arg(FILENAMESIZE)
 integer getarg, ctoi
 integer i, j

 include cisam

 dif = DEFAULT_DIF
 width = DEFAULT_WIDTH
 justfy = DEFAULT_JUSTFY

 for (i=1; getarg(i, arg, FILENAMESIZE) != EOF; i=i+1)
    {
    call fold(arg)
    if (arg(1) == MINUS)
            if (arg(2) == LETD)
                {
                j = 3
                dif = ctoi(arg, j)
                if (dif <= 0)
                    dif = DEFAULT_DIF
                }
            else if (arg(2) == LETW)
                {
                j = 3
                width = ctoi(arg, j)
                if (width <= 0)
                    width = DEFAULT_WIDTH
                }
            else if (arg(2) == LETJ)
                {
                if (arg(3) == LETL)
                    justfy = LEFT
                else if (arg(3) == LETR)
                    justfy = RIGHT
                }
            else
                call badarg(arg)
    else
        call badarg(arg)
    }

 return
 end
 subroutine outlin(word, addrs)

 character word(ARB)
# linepointer addrs
  integer addrs(2)

 include cisam

 if (justfy == RIGHT)
    call putstr(word, width, STDOUT)
 else
    call putstr(word, -width, STDOUT)
 call putc(BLANK)
 call putptr(addrs, STDOUT)
 call putc(NEWLINE)

 return
 end
 ## ptrcpy - copy pointer
 subroutine ptrcpy (in, out)
 integer in(2), out(2)
 # Assume pointers occupy 2 integer words
 
 if (in(1) == NULLPOINTER)
	{
	out(1) = NULLPOINTER
	out(2) = NULLPOINTER
	}
 else
	{
	out(1) = in(1)
	out(2) = in(2)
	}
 return
 end
## putptr - print pointer on output
 subroutine putptr (addrs, fd)
 
 # Assume pointers occupy 2 integer words
 integer addrs(2), fd
 include cisam
 integer iw
 
 if (width == 1)
	iw = 0
 else
	iw = 6
 call putint (addrs(1), iw, fd)
 call putch (BLANK, fd)
 call putint (addrs(2), iw, fd)
 return
 end

# ptreq - compare pointers
integer function ptreq (a,b)
integer a(2), b(2)
if (a(1) == b(1) & a(2) == b(2))
	return(YES)
return(NO)
end

# badarg - complain and quit of arg error
 subroutine badarg(arg)
 character arg(ARB)
 call putlin(arg,STDERR)
 call error(" illegal arg.")
 return
 end
