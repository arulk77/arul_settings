sh ./make_cscope.sh /home/vbonagiri/host /home/vbonagiri/host/cscope_tag_files/tags /home/vbonagiri/host/cscope_tag_files/list_cscope_project_dirs.txt
#Note:
#Don't forget to change below line to ~/.vimrc file, if project directory changed. 
#map cpa :cscope add /home/vbonagiri/host/cscope_tag_files/tags/<C-R>=expand("<cword>")<CR>/cscope.out.
