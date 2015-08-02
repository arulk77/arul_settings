make_cscope()
{
	PROJ_NUM=$1
	SRC_SUB_DIR=$2
	PROJ_SUB_DIR=$3/$1

	mkdir -p $PROJ_SUB_DIR
	cd $PROJ_SUB_DIR > /dev/null
	echo "Project Directory $PROJ_SUB_DIR Created."
	find $SRC_SUB_DIR -name "*.[chsS]" > cscope.files
	find $SRC_SUB_DIR -name "Makefile*" >> cscope.files
	find $SRC_SUB_DIR -name "Kconfig" >> cscope.files
	cscope -b -q -k -i cscope.files
	cd - > /dev/null
}

if [ "$1" == "" ] || [ "$2" == "" ] || [ "$3" == "" ]; then
	echo "Help: make_cscope.sh <SRC_DIR> <PROJ_DIR> <PROJ_LIST_FILE>"
else
	SRC_DIR=$1
	PROJ_DIR=$2
	PROJ_LIST_FILE=$3
	i=1

	printf "PROJ_NUM\tPATH\n" > $SRC_DIR/project_list.txt
	mkdir -p $PROJ_DIR
	while read line
	do
        	echo "Creating Cscope Project:$i for Path: $SRC_DIR/$line"
		printf  "%5s %s\n" $i $SRC_DIR/$line >> $SRC_DIR/project_list.txt
		make_cscope $i $SRC_DIR/$line $PROJ_DIR 
		i=$(expr $i + 1)
		echo "done."
		echo ""
	done <$PROJ_LIST_FILE
fi
