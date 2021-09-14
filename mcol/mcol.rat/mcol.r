# mcol - format standard input into multiple columns

 # include ratdef
 define(COLUMNS,2)      # defaults
 define(PAGESIZE,55)
 define(GUTTER,8)
 define(LINESIZE,60)
 define(MAXBUF,7000)    # size limits
 define(MAXPTR,1200)

 DRIVER(mcol)
   integer pagsiz, linsiz, ncols, gutsiz, lineno, nlines, i, j, fd
   integer readln, ctoi, getarg, mod, max, open
   character arg(MAXLINE)
   include cmcol

   for (i = 1; i <= MAXPTR; i = i + 1)  # clear pointer array
      linptr(i) = 0
   col = 0
   nextbf = 1
   pagsiz = PAGESIZE            # set defaults
   linsiz = LINESIZE
   ncols = COLUMNS
   gutsiz = GUTTER
   fd = ERR
   call query ("usage: mcol [-cn] [-ln] [-wn] [-gn] [-dn] [file].")
   for (i = 1; getarg(i, arg, MAXLINE) ^= EOF; i = i + 1) {
      if (arg(1) == MINUS & arg(2) ^= EOS)
          call colarg (arg, pagsiz, ncols, gutsiz, linsiz)
      else if (arg(1) == MINUS & arg(2) == EOS)
          call docol (pagsiz, ncols, gutsiz, linsiz, STDIN)
       else
             {
             fd = open(arg, READ)
             if (fd == ERR)
                        call cant(arg)
             call docol(pagsiz, ncols, gutsiz, linsiz, fd)
             call close (fd)
             }
         }
   if (fd == ERR)        #read STDIN
         call docol (pagsiz, ncols, gutsiz, linsiz, STDIN)
    DRETURN
   end
 ## colarg - process flags for mcol tool
 subroutine colarg (arg, pagsiz, ncols, gutsiz, linsiz)
 integer pagsiz, ncols, gutsiz, linsiz, j
 integer ctoi
 character arg(ARB)

 j = 3
 j = ctoi(arg, j)
 if (arg(2) == LETC | arg(2) == BIGC)
        {
         ncols = j
         if (ncols <= 0)
                call error ("invalid column count.")
        }
 else if (arg(2) == LETL | arg(2) == BIGL)
        {
        pagsiz = j
        if (pagsiz <= 0)
                call error ("invalid page size.")
        }
  else if ( (arg(2) == LETW | arg(2) == BIGW) |
            (arg(2) == LETS | arg(2) == LETS) )  #UofA convention
        {
        linsiz = j
        if (linsiz <= 0)
                call error ("invalid column width.")
        }
 else if (arg(2) == LETG | arg(2) == BIGG)
        {
        gutsiz = j
        if (gutsiz < 0)
                call error ("invalid gutter width.")
        }
 else if (arg(2) == LETD | arg(2) == BIGD)
        {
        pagsiz = 23             # display defaults
        linsiz = 10
        ncols = 7
        gutsiz = 1
        if (j > 0)   # set column width and number of columns
                {
                linsiz = j
                ncols = max(1, 81/(linsiz+1))
                if (ncols > 1)
                        {
                        gutsiz = (79 - (linsiz+1)*ncols)/(ncols - 1)+1
                        if (gutsiz <= 0)
                                ncols = ncols - 1
                        }
                }
        }
 else
        call remark ("ignoring invalid flag.")
 return
 end
 ## colerr - print error in mcol usage and stop
 subroutine colerr

 call error ("usage: mcol [-cn] [-ln] [-wn] [-gn] [-dn] [file] .")
 return
 end
 ## docol - process file for mcol
 subroutine docol (pagsiz, ncols, gutsiz, linsiz, fd)
 integer pagsiz, ncols, gutsiz, linsiz, fd, nlines, lineno, i
 integer readln
 include cmcol

 nlines = pagsiz*ncols  # total number of lines/page
 if (nlines > MAXPTR)
        call error ("too many lines.")
 for (lineno = 1; readln(i, linsiz, fd) ^= EOF; lineno = lineno + 1)
      {
      call inject(i, lineno)
      if (lineno >= nlines) {
         call outbuf(pagsiz, linsiz, gutsiz)
         lineno = 0
         }
      }
   if (lineno > 1) {
      pagsiz = lineno/ncols
      if (mod(lineno, ncols) ^= 0)
         pagsiz = pagsiz + 1
      call outbuf(pagsiz, linsiz, gutsiz)
      }
 return
 end
# inject - insert pointer ptr into linptr array
   subroutine inject(ptr, lineno)
   integer ptr, lineno
   include cmcol

   if (lineno > MAXPTR)
      call error("insufficient buffer space.")
   linptr(lineno) = ptr
   return
   end
# outbuf - dump current buffer to formatted page
   subroutine outbuf(pagsiz, linsiz, gutsiz)
   integer pagsiz, linsiz, gutsiz
   integer i, j, k
   include cmcol

   for (i = 1; linptr(i) ^= 0; i = i + 1) {
#      call outlin(linbuf(linptr(i)))
       k = linptr(i)
       call outlin (linbuf(k))
      linptr(i) = 0
      for (j = i + pagsiz; linptr(j) ^= 0; j = j + pagsiz) {
#         call outtab((linsiz + gutsiz)*((j - 1)/pagsiz))
	  call outtab((linsiz + gutsiz) * max((j-1)/pagsiz,1) )
#         call outlin(linbuf(linptr(j)))
          k = linptr(j)
          call outlin (linbuf(k))
         linptr(j) = 0
         }
      call outch(NEWLINE)
      }
   nextbf = 1
   return
   end
# outch - output c to formatted page
   subroutine outch(c)
   character c
   include cmcol

   call putc(c)
   if (c == NEWLINE)
      col = 0
   else
      col = col + 1
   return
   end
# outlin - output str to formatted page
   subroutine outlin(str)
   character str(ARB)
   integer i

   for (i = 1; str(i) ^= EOS; i = i + 1)
      call outch(str(i))
   return
   end
# outtab - tab to column n on formatted page
   subroutine outtab(n)
   integer n
   include cmcol

   while (col < n)
      call outch(BLANK)
   return
   end
# readln - read next line (<= linsiz) into linbuf; return location p
   integer function readln(p, linsiz, fd)
   integer p, linsiz, fd
   integer i
   character getch
   character c
   include cmcol

   p = nextbf
   for (i = 1; getch(c, fd) ^= EOF; i = i + 1) {
      if (c == NEWLINE)
         break
      if (i <= linsiz) {
         if (nextbf >= MAXBUF)
            call error("insufficient buffer space.")
         linbuf(nextbf) = c
         nextbf = nextbf + 1
         }
      }
   if (c == EOF & i == 1)
      return (EOF)
   linbuf(nextbf) = EOS
   nextbf = nextbf + 1
   return (i - 1)
   end
