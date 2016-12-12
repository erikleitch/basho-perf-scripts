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

sep()
{
    s=$1
    sep=$2
    case $s in
	(*"$sep"*)
	    before=${s%%"$sep"*}
	    after=${s#*"$sep"}
	    ;;
	(*)
	    before=$s
	    after=
	    ;;
    esac
    after=${after//\"/\\\"}
    echo "\"$before\" \"$after\""
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
    riak=$(valOrDef riak '' "$@")
    riak=${riak//\"/}
    unset ERL_AFLAGS
    export CURR_DIR=`pwd`
    echo "-pa ${CURR_DIR}/.eunit ${CURR_DIR}/ebin -pa ${CU\
RR_DIR}/deps/*/ebin -pa riak_ee/deps/*/ebin -pa ${RIAK_TES\
T_BASE}/erlang_scripts/*/ebin -pa $riak/lib/*/ebin"
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
    riak=$(valOrDef riak '' "$@")
    
    flags=$(erlt_flags riak=$riak)
    erl $flags -noshell -run ${mod//\"/} ${fn//\"/} ${args//\"/} -run init stop
}

echoerr()
{
    printf "$@" 1>&2;
}

getColorizedString()
{
    local str=$1
    local color=$2
    GREEN="\033[32m"
    YELLOW="\033[33m"
    LIGHTRED="\033[91m"
    NORM="\033[0m"

    case $color in
	"green")
	    outStr=$GREEN$str$NORM
	    echo "$outStr"
	    ;;
	"yellow")
	    outStr=$YELLOW$str$NORM
	    echo "$outStr"
	    ;;
	"lightred")
	    outStr=$LIGHTRED$str$NORM
	    echo "$outStr"
	    ;;
	"red")
	    outStr=$LIGHTRED$str$NORM
	    echo "$outStr"
	    ;;
	*)
	    ;;
    esac
}

colorize()
{
    local inStr=$1
    local color=$2

    echo -e $(getColorizedString "$inStr" "$color")
}

annotate ()
{
    line=$1;
    color=$2;
    colorize "============================================================" $color;
    colorize "$line" $color;
    colorize "============================================================" $color
}


