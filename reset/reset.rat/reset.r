 DRIVER(reset)
 character args(MAXLINE)
 string pmpt "'return' when ready "

 call mkarg$ (args)
 if (args(1) != EOS)
	{
	call outl$ (args)
	call prompt (pmpt, args, STDIN)
	}
 call bdos$d (13, 0)
 DRETURN
 end
