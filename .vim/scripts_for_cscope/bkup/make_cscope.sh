find ./ -name "*.[chsS]" > cscope.files 
find ./ -name "Makefile*" >> cscope.files
find ./ -name "Kconfig" >> cscope.files
cscope -b -q -k 
