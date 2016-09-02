#=======================================================================
# Arg parsing functions
#=======================================================================

#------------------------------------------------------------
# Given a possible list of var=val args, return the specified value
# for a named parameter, or its default value
#
# Use like:
#
#     valOrDef myvar defval              (returns defval)
#     valOrDef myvar defval myvar=newval (returns newval)
#
# This version works with arguments that are arbitrary quoted strings
# as well, ie:
#
#     valOrDef myvar defval myvar="this is a test"
#
# will return:
#
#    "this is a test"
#
#------------------------------------------------------------

valOrDef()
{
    argname=$1
    defval=$2

    shift 2

    retval=\"$defval\"
    for i
    do
	keyval=($(sep "$i" "="))
	key=${keyval[0]}
	key=${key//\"/}
	val=${keyval[@]: 1}

	case $key in
	    $argname)
		retval=${val//^/ }
		;;
	    *)
		;;
	esac

    done
    echo $retval
}

#------------------------------------------------------------
# Return the most recent file in the specified directory
#------------------------------------------------------------

getlast()
{
    dir=$1

    unset files
    unset times
    
    iFile=0
    for file in $dir/*.txt
    do 
	times[iFile]=`stat -t %s $file | awk '{print $10}'`
	files[iFile]=$file
	iFile=$[$iFile+1]
    done

    arr2=( $(
	    for el in "${times[@]}"
	    do
		echo "$el"
	    done | sort -r) )

    latest=${arr2[0]}

    iFile=0
    for el in "${files[@]}"
    do
	if [ ${times[iFile]} = $latest ] 
	then
	    file=${files[iFile]}
	    echo $file
	fi
	iFile=$[$iFile+1]
    done
}

#=======================================================================
# Functions for running erlang 
#=======================================================================

#------------------------------------------------------------
# Set up erl flags to find relevant modules
#------------------------------------------------------------

erlt_flags() {
    unset ERL_AFLAGS
    export CURR_DIR=`pwd`
    echo "-pa ${CURR_DIR}/.eunit ${CURR_DIR}/ebin -pa ${CURR_DIR}/deps/*/ebin -pa ${CURR_DIR}/lib/*/ebin -pa riak_ee/deps/*/ebin -pa ${RIAK_TEST_BASE}/erlang_scripts/*/ebin"
}

erlt_fn() {
    unset ERL_AFLAGS
    export CURR_DIR=`pwd`
    alias erlt="erl -pa ${CURR_DIR}/.eunit ${CURR_DIR}/ebin -pa ${CURR_DIR}/deps/*/ebin -pa ${CURR_DIR}/lib/*/ebin -pa riak_ee/deps/*/ebin -pa ${RIAK_TEST_BASE}/erlang_scripts/*/ebin"
}

#------------------------------------------------------------
# Run and erlang module and function
#------------------------------------------------------------

runerl()
{
    mod=$(valOrDef mod '' "$@")
    fn=$(valOrDef fn '' "$@")
    args=$(valOrDef args '' "$@")

    flags=$(erlt_flags)
    erl $flags -noshell -run ${mod//\"/} ${fn//\"/} ${args//\"/} -run init stop
}
