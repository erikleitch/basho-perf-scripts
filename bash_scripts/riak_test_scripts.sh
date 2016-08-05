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

#=======================================================================
# Functions for running erlang 
#=======================================================================

runerl()
{
    mod=$(valOrDef mod '' "$@")
    fn=$(valOrDef fn '' "$@")
    args=$(valOrDef args '' "$@")

    flags=$(erlt_flags)
    erl $flags -noshell -run ${mod//\"/} ${fn//\"/} ${args//\"/} -run init stop
}

#=======================================================================
# Functions for running riak_test
#=======================================================================

#------------------------------------------------------------
# From inside a riak_ee tree, recreate a devrel
#------------------------------------------------------------

redev_fn()
{
    if [ $# -eq 0 ]
    then
	echo "Recreate a devrel, from top of a riak_ee tree"
	echo ""
	echo "Use like: redev_fn riakTestDir nNodes"
	return
    fi
    
    riakTestDir=$1
    nNode=$2
    \rm -rf dev
    make devrel DEVNODES=$nNode
    $riakTestDir/bin/rtdev-current.sh
}

#------------------------------------------------------------
# Meant to be used from a riak_test checkout containing a riak_ee tree
#------------------------------------------------------------

rerun()
{
    if [ $# -eq 0 ]
    then
	echo "Rerun a riak_test script, from a riak_test checkout containing riak_ee, recompiling riak_ee and recreating the devrel"
	echo ""
	echo "Use like: rerun script=testName [target=makeTarget] [nodes=nNodes] [args=riakTestArgs]"
	return
    fi

    riakTestDir=`pwd`
    cd riak_ee

    target=$(simpleValOrDef target '' $@)
    script=$(simpleValOrDef script '' $@)
    args=$(simpleValOrDef args '' $@)
    nodes=$(simpleValOrDef nodes '1' $@)

    if [ ! -z $target]
    then
	annotate "Making riak_ee with target='$target'" "green"
	make $target
    fi

    redev_fn $riakTestDir $nodes
    cd $riakTestDir

    annotate "Running riak_test with script='$script'" "green"
    riaktest $script "$args"
#    riaklog
}

#------------------------------------------------------------
# Run a riak_test test
#------------------------------------------------------------

riaktest()
{
    if [ $# -eq 0 ]
    then
	echo "Run a riak_test script, from a riak_test checkout"
	echo ""
	echo "Use like: riaktest testName [riakTestArgs]"
	return
    fi

    if [ $# -eq 1 ]
    then
	./riak_test -c riak_ts -t $1
    else
	./riak_test -c riak_ts -t $1 $2
    fi
}

#------------------------------------------------------------
# Rerun the KV latency test sequence
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

parseKvLatencyTestProfilerOutput()
{
    fileName=$1

    local pycomm+="import numpy as np\n"
    pycomm+="import matplotlib.pyplot as plt\n"
    pycomm+="\n"
    pycomm+="def getLine(label, content):\n"
    pycomm+="  for line in content:\n"
    pycomm+="    if line.split(' ')[0] == label:\n"
    pycomm+="      return line.split(' ')\n"
    pycomm+="  return []\n"
    pycomm+="\n"
    pycomm+="def parseProfilerOutput(fileName, labelDict):\n"
    pycomm+="\n"
    pycomm+="  with open(fileName) as f:\n"
    pycomm+="    content = f.readlines()\n"
    pycomm+="\n"
    pycomm+="    nline = len(content)\n"
    pycomm+="\n"
    pycomm+="    totalcount = getLine('totalcount', content)\n"
    pycomm+="    labels     = getLine('label',      content)\n"
    pycomm+="    counts     = getLine('count',      content)\n"
    pycomm+="    usec       = getLine('usec',       content)\n"
    pycomm+="\n"
    pycomm+="  if len(labels) != 0:\n"
    pycomm+="    for i in range(1, len(labels)):\n"
    pycomm+="      label = labels[i].replace(\"'\", \"\")\n"
    pycomm+="      label = label.replace(\"\\\n\", \"\")\n"
    pycomm+="\n"
    pycomm+="      if len(label) > 0:\n"
    pycomm+="        labelDict[label] = {}\n"
    pycomm+="        labelDict[label]['usec']  = float(usec[i+1])\n"
    pycomm+="        labelDict[label]['count'] = int(counts[i+1])\n"
    pycomm+="  return labelDict\n"
    pycomm+="\n"
    pycomm+="def getProfilerOutput(fileName):\n"
    pycomm+="  d = parseProfilerOutput(fileName, {})\n"
    pycomm+="  gbytes = []\n"
    pycomm+="  pbytes = []\n"
    pycomm+="  getus = []\n"
    pycomm+="  putus = []\n"
    pycomm+="  for key in d.keys():\n"
    pycomm+="    s = key.split('_')\n"
    pycomm+="    if s[0] == 'put':\n"
    pycomm+="      pbytes.append(float(s[1]))\n"
    pycomm+="      ntrial = int(s[2])\n"
    pycomm+="      putus.append(d[key]['usec']/ntrial)\n"
    pycomm+="    elif s[0] == 'get':\n"
    pycomm+="      gbytes.append(float(s[1]))\n"
    pycomm+="      ntrial = int(s[2])\n"
    pycomm+="      getus.append(d[key]['usec']/ntrial)\n"
    pycomm+="  btes = []\n"
    pycomm+="  puts = []\n"
    pycomm+="  gets = []\n"
    pycomm+="  ps = np.argsort(pbytes)\n"
    pycomm+="  gs = np.argsort(gbytes)\n"
    pycomm+="  for i in range(0,np.size(ps)):\n"
    pycomm+="    btes.append(pbytes[ps[i]])\n"
    pycomm+="    puts.append(putus[ps[i]])\n"
    pycomm+="    gets.append(getus[gs[i]])\n"
    pycomm+="\n"
    pycomm+="  return btes,puts,gets\n"
    pycomm+="\n"
    pycomm+="def makePlot(bytes, puts, gets):\n"
    pycomm+="  fig = plt.figure()\n"
    pycomm+="  fig.set_facecolor('white');\n"
    pycomm+="  ax = fig.add_subplot(1,1,1)\n"
    pycomm+="  plt.plot(bytes,gets)\n"
    pycomm+="  plt.hold(True)\n"
    pycomm+="  plt.plot(bytes,puts,'m')\n"
    pycomm+="  ax.set_xscale('log')\n"
    pycomm+="  ax.set_yscale('log')\n"
    pycomm+="  plt.xlabel('Data size (bytes)',fontsize=16)\n"
    pycomm+="  plt.ylabel('Latency (\$\mu\$sec)',fontsize=16)\n"
    pycomm+="  plt.legend(['Riak KV Get Latency', 'Riak KV Put Latency'],loc='upper left')\n"
    pycomm+="  plt.title('Round-trip Riak KV Latencies',fontsize=16)\n"
    pycomm+="  plt.show()\n"
    pycomm+="\n"
    pycomm+="bytes, puts, gets = getProfilerOutput('$fileName')\n"
    pycomm+="makePlot(bytes, puts, gets)\n"

    printf "$pycomm" > /tmp/pytest.py
    printf "$pycomm" | python
}

runKvLatencyTest()
{
    disp=$(valOrDef disp 'false' "$@")
    disp=${disp//\"/}

    runerl mod=mrts fn=runKvLatencyTests

    if [ $disp == "true" ]; then
	parseKvLatencyTestProfilerOutput `getlast /tmp/client_profiler_results`
    fi
}

kvLatencyTestSequence()
{
    # Set up devrel for 3-node cluster, and run the riak_test script
    # to create the cluster
    
    rerun script=ts_setup_kv_nval3 args=--keep nodes=3
    runKvLatencyTest disp=false
    cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval3.txt

    # Create with w1c bucket, and rerun latency tests

    riaktest ts_setup_kv_nval3_w1c --keep
    runKvLatencyTest disp=false
    cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval3_w1c.txt

    # Create with nval1, and rerun latency tests

    riaktest ts_setup_kv_nval1 --keep
    runKvLatencyTest disp=false
    cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval1.txt

    # Create with nval1, w1c and rerun latency tests

    riaktest ts_setup_kv_nval1_w1c --keep
    runKvLatencyTest disp=false
    cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval1_w1c.txt

    # Create with nval3, bitcask

    riaktest ts_setup_kv_nval3_bitcask --keep
    runKvLatencyTest disp=false
    cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval3_bitcask.txt

    # Create with nval3, bitcask

    riaktest ts_setup_kv_nval1_bitcask --keep
    runKvLatencyTest disp=false
    cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval1_bitcask.txt

    python $RIAK_TEST_BASE/python_scripts/kvlatency_cmp.py
}

