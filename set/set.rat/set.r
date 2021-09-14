## set - set an item in the environment file
DRIVER(set)
character line(MAXLINE)
character value(MAXLINE)
integer getarg, i
integer delete, verbos
string usage "usage: set [-v] [-d] item value(s)"

data delete /NO/
data verbos /NO/

call query (usage)

if (getarg (1, line, MAXLINE) == EOF)
	call error (usage)
if (line(1) == MINUS)
	{
	if (line(2) == LETV | line(2) == BIGV)
		verbos = YES
	else if (line(2) == LETD | line(2) == BIGD)
		delete = YES
	for (i=2; getarg (i, line, MAXLINE) != EOF; i=i+1)
		call doit (line, EOS, delete, verbos)
	if (i == 2 & verbos == YES)
		call prtenv
	}

else
	{
	call delarg (1)
	call getcl (value, MAXLINE)
	call doit (line, value, NO, NO)
	}
if (verbos == NO)		# there were some changes made
	call setenv
DRETURN
end
## doit - perform operation on environment file
subroutine doit (name, value, delete, verbos)
character name(ARB), value(ARB)
character buf(MAXLINE)
integer delete, verbos, i, ctoi, ctoc, getwrd, equal
string equals " = "
string on "ON"
string off "OFF"
string err "invalid item name: "
string items "PATH TEMP MAXDEV DATE TAB HOME EXT LIST VERBOSE TTCOLS TTROWS ATEND CTRLC"
#  PATH TEMP   MAXDEV DATE  TAB    HOME   EXT  LIST VERBOSE TTCOLS TTROWS ATEND CTRLC
#  1    2      3      4     5      6      7    8    9       10     11     12 13
#  c    c      c      i     b      c      c     b   b        b     b      c b
# path  tmpdef maxdev ddate tabsiz homedr extn list vbose   ttcols ttrows atend ctrlc

define(do_char,	{if (delete == YES)  $1(1) = EOS
else if (verbos == YES)
{call putlin (name, STDOUT)
call putlin(equals,STDOUT)
call putlin ($1, STDOUT)
call putch (NEWLINE, STDOUT)}
else  i=ctoc(value,$1,MAXLINE) } )

define(do_int,	{if (delete == YES)  $1 = 0
else if (verbos == YES)
{call putlin (name, STDOUT)
call putlin(equals,STDOUT)
i = $1
call putint (i, 1, STDOUT)
call putch (NEWLINE, STDOUT)}
else  {i = 1; $1 = ctoi(value,i)} } )

define(do_onoff, {if (verbos == NO)
{if (equal(value, on) == YES)  value(1) = DIG1
else if (equal(value, off) == YES)  value(1) = DIG0
else  {call remark ("invalid value.");  return}
value(2) = EOS}
do_int($1) } )

include config
include cntrlc


call upper (name)
call upper (value)
i = 1
for (j=1; getwrd (items, i, buf) != 0; j=j+1)
	{
	if (equal (buf, name) == YES)	# found it
		break
	}
switch (j)
	{
	case 1: do_char(path)
	case 2: do_char(tmpdev)
	case 3:	{
		if (verbos == YES)
			{
			call putlin (name, STDOUT)
			call putlin (equals, STDOUT)
			call putch (maxdev, STDOUT)
			call newlin (STDOUT)
			}
		else if (value(1) < BIGA | value(1) > BIGP)
			{
			call remark ("illegal maxdev value.")
			return
			}
		else
			maxdev = value(1)
		}
	case 4: call remark ("use 'date' tool to set and show date.")
	case 5: do_int(tabsiz)
	case 6: do_char(homedr)
	case 7: do_char(extn)
	case 8: do_onoff(list)
	case 9: do_onoff(vbose)
	case 10: do_int(ttcols)
	case 11: do_int(ttrows)
	case 12: do_char(atend)
	case 13: do_onoff(ctrlc)
	default: call putlin (err, ERROUT)
		call putlin (name, ERROUT)
		call putch (NEWLINE, ERROUT)
	}
return
end
## prtenv - print environment file
subroutine prtenv
integer open, fd, getlin
character line (MAXLINE)
include "cshstuff"

fd = open (envfil, READ)
if (fd == ERR)
	call cant (envfil)
while (getlin(line, fd) != EOF)
	call putlin (line, STDOUT)
call close (fd)
return
end


## evchar - change/print character item in environment
#subroutine evchar (name, j, value, delete, verbos)
#character name(ARB)
#
#integer j		# index of item
##  PATH TEMP   MAXDEV DATE  TAB    HOME   EXT  LIST VERBOSE TTCOLS TTROWS ATEND
##  1    2      3      4     5      6      7    8    9       10     11     12
##  c    c      c      i     b      c      c     b   b        b     b      c
## path  tmpdef maxdev ddate tabsiz homedr extn list vbose   ttcols ttrows atend
#
#character value(ARB)
#integer delete, verbos
#
#if (delete == YES)
#	{
#	switch (j)
#		{
#		case 1: path(1) = E0S
#		case 2: tmpdef(1) = EOS
#		case 3: maxdev(1) = EOS
#		case 6: homedr(1) = EOS
#		case 7: extn(1) = EOS
#		case 12: atend(1) = EOS
#		}
#	}
#else if (verbos == YES)
#{call putlin (name, STDOUT)
#call putlin (" = ", STDOUT)
#call putlin ($1, STDOUT)
#call putch (NEWLINE, STDOUT)}
#else
#i=ctoc(value,$1,MAXLINE) )
#
