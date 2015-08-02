while read line
do
        echo "creating cscope project for: $line"
	#cp ~/.vim/scripts_for_cscope/make_cscope.sh $line
	#cp ~/.vim/scripts_for_cscope/clean_cscope.sh $line
        #cd $line
	#./make_cscope.sh
        #cd - > /dev/null
	echo "done."
	echo ""
done <list_cscope_project_dirs.txt
