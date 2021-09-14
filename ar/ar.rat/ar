## ar - archive file maintainer

# This archiver can read archives in various formats.  When
#     it finds a file size in a header (either a character
#     count or a seek offset in local format), it uses that
#     size to read or to skip over the archive element.  Then
#     it expects either an end of file, a valid header, or a
#     trailer to match this element's header.  If there is no
#     no file size recorded in the header, then it simply scans
#     for the proper matching trailer.  In this way the
#     archiver exploits any available file size information
#     without necessarily insisting upon it.
# Option 'r' forces the archiver to ignore file sizes and to
#     infer file boundaries from header-trailer matchups alone.
# It is assumed that if ANY archive element carries a trailer
#     then ALL of them do.  Exceptions trigger error messages.

   DRIVER(ar)

   include carch

   character aname (FILENAMESIZE), comand (MAXARG), cmd(MAXARG)
   integer ifd, ifp, ift, ifu, ifx, ifv, ifr
   data ifd, ifp, ift, ifu, ifx, ifv, ifr / 7*0 /
   integer getarg, length, index
   string usage USAGE_MESSAGE

   errcnt = 0

   call query (usage)

   if (getarg (1, comand, MAXARG) == EOF
     | getarg (2, aname, FILENAMESIZE) == EOF)
      call error (usage)

   call getfns

   if (comand(1) == MINUS)
	    call scopy (comand, 2, cmd, 1)	# without MINUS
	else
	    call scopy (comand, 1, cmd, 1)

   call fold (cmd)

   if (index(cmd, VERBOSE_CMD) > 0)
	    {
	    ifv = 1
	    verbos = YES
	    }
	else
	    verbos = NO
   if (index(cmd, READ_CMD) > 0)
	    {
	    ifr = 1
	    rswich = YES
	    ccount = CLINES
	    intrls = YES
	    }
	else
	    {
	    rswich = NO
	    intrls = DONT_KNOW
	    }
   if (index(cmd, DELETE_CMD) > 0)
	    ifd = 1
   if (index(cmd, PRINT_CMD) > 0)
	    ifp = 1
   if (index(cmd, TABLE_CMD) > 0)
	    ift = 1
   if (index(cmd, UPDATE_CMD) > 0)
	    ifu = 1
   if (index(cmd, EXTRACT_CMD) > 0)
	    ifx = 1

   if (ifd + ifp + ift + ifu + ifx != 1
	| ifr + ifv + 1 != length(cmd))
	    call error (usage)
   if (ifu == 1)
      call update (aname)
   else if (ift == 1)
      call table (aname)
   else if (ifx == 1)
      call extrac (aname, EXTRACT_CMD)
   else if (ifp == 1)
      call extrac (aname, PRINT_CMD)
   else if (ifd == 1)
      call delete (aname)
   else
      call error ("error in main:  can't happen.")

   if (errcnt != 0)
      call error (".")

   DRETURN
   end
