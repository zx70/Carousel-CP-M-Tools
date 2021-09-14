## logout - clean up shell files and return to local system
DRIVER(logout)
include config
integer flag

flag = NO
if (atend(1) != EOS)
	{
	flag = YES
	atend(1) = EOS
	}
if (list == 1)
	{
	flag = YES
	list = 0
	}
if (vbose == 1)
	{
	flag = YES
	vbose = 0
	}
if (flag == YES)
	call setenv
call remove (shcmd)
call exit
return
end
