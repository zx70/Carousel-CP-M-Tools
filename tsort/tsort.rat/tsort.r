define(MAXBUF,5000)     # storage array
define(MAXSYMBOL,120)   # maximum symbol size
# symbol table entries
define(NEXT,0)          # pointer to next entry
define(SYMBOL,1)        # pointer to symbol structure
define(CHARS,2)         # characters in symbol
# node structure
define(LINK,0)          # pointer to next node
define(SUCC,1)          # pointer to successor symbol structure
define(NODESIZE,2)      # size of node structure
# symbol structure
define(NAME,0)          # symbol structure; pointer to name
define(COUNT,1)         # successor count
define(TOP,2)           # beginning of successor list
define(SYMSIZE,3)       # size of symbol structure

DRIVER
 
 integer getarg, open
 integer i, fd
 character arg(FILENAMESIZE)
 
 for (i=1; getarg(i, arg, FILENAMESIZE) != EOF; i=i+1)
        {
        if (arg(1) == QMARK & arg(2) == EOS)
                call usage
        else if (arg(1) == MINUS & arg(2) == EOS)
                fd = STDIN
        else
                {
                fd = open(arg, READ)
                if (fd == ERR)
                        call cant(arg)
                }
        call tpsort (fd)
        if (fd != STDIN)
                call close(fd)
        }
 if (i == 1)            #read STDIN
        call tpsort (STDIN)
 DRETURN
 end
