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

    target=$(simpleValOrDef target 'locked-all' $@)
    script=$(simpleValOrDef script '' $@)
    args=$(simpleValOrDef args '' $@)
    nodes=$(simpleValOrDef nodes '1' $@)

    annotate "Making riak_ee with target='$target'" "green"
    make $target

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
# Parse output from a single KV latency test sequence and plot it
#------------------------------------------------------------

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
    pycomm+="  dbytes = []\n"
    pycomm+="  getus = []\n"
    pycomm+="  putus = []\n"
    pycomm+="  delus = []\n"
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
    pycomm+="    elif s[0] == 'del':\n"
    pycomm+="      dbytes.append(float(s[1]))\n"
    pycomm+="      ntrial = int(s[2])\n"
    pycomm+="      delus.append(d[key]['usec']/ntrial)\n"
    pycomm+="  btes = []\n"
    pycomm+="  puts = []\n"
    pycomm+="  gets = []\n"
    pycomm+="  dels = []\n"
    pycomm+="  ps = np.argsort(pbytes)\n"
    pycomm+="  gs = np.argsort(gbytes)\n"
    pycomm+="  ds = np.argsort(dbytes)\n"
    pycomm+="  for i in range(0,np.size(ps)):\n"
    pycomm+="    btes.append(pbytes[ps[i]])\n"
    pycomm+="    puts.append(putus[ps[i]])\n"
    pycomm+="    gets.append(getus[gs[i]])\n"
    pycomm+="    dels.append(delus[gs[i]])\n"
    pycomm+="\n"
    pycomm+="  return btes,puts,gets,dels\n"
    pycomm+="\n"
    pycomm+="def makePlot(bytes, puts, gets, dels):\n"
    pycomm+="  fig = plt.figure()\n"
    pycomm+="  fig.set_facecolor('white');\n"
    pycomm+="  ax = fig.add_subplot(1,1,1)\n"
    pycomm+="  plt.plot(bytes,gets)\n"
    pycomm+="  plt.hold(True)\n"
    pycomm+="  plt.plot(bytes,puts,'m')\n"
    pycomm+="  plt.plot(bytes,dels,'y')\n"
    pycomm+="  ax.set_xscale('log')\n"
    pycomm+="  ax.set_yscale('log')\n"
    pycomm+="  plt.xlabel('Data size (bytes)',fontsize=16)\n"
    pycomm+="  plt.ylabel('Latency (\$\mu\$sec)',fontsize=16)\n"
    pycomm+="  plt.legend(['Riak KV Get Latency', 'Riak KV Put Latency', 'Riak KV Del Latency'],loc='upper left')\n"
    pycomm+="  plt.title('Round-trip Riak KV Latencies',fontsize=16)\n"
    pycomm+="  plt.show()\n"
    pycomm+="\n"
    pycomm+="bytes, puts, gets, dels = getProfilerOutput('$fileName')\n"
    pycomm+="makePlot(bytes, puts, gets, dels)\n"

    printf "$pycomm" > /tmp/pytest.py
    printf "$pycomm" | python
}

#-----------------------------------------------------------------------
# Run a single iteration of the KV latency test
#-----------------------------------------------------------------------

runKvLatencyTest()
{
    disp=$(valOrDef disp 'false' "$@")
    disp=${disp//\"/}

    runerl mod=riak_prof_tests fn=runKvLatencyTests

    if [ $disp == "true" ]; then
	parseKvLatencyTestProfilerOutput `getlast /tmp/client_profiler_results`
    fi
}

#-----------------------------------------------------------------------
# Run a complete sequence of KV latency tests, doing 'iter' iterations
# of each step, starting at index 'start'.  Files will be saved with
# prefix 'prefix'
#-----------------------------------------------------------------------

kvLatencyTestSequence()
{
    nIter=$(simpleValOrDef iter '1' $@)
    startIter=$(simpleValOrDef start '0' $@)
    prefix=$(simpleValOrDef prefix 'kv' $@)
    
    endIter=$[$startIter + $nIter]

    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi

    \rm riak_ee
    ln -s branches/riak_riak_2.1.4 riak_ee

    #------------------------------------------------------------
    # Set up devrel for 3-node cluster, and run the riak_test script
    # to create the cluster
    #------------------------------------------------------------
    
    rerun script=ts_setup_kv_nval3 args=--keep nodes=3

    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runKvLatencyTest disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval3_"$prefix"_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    #------------------------------------------------------------
    # Create with w1c bucket, and rerun latency tests
    #------------------------------------------------------------

    riaktest ts_setup_kv_nval3_w1c --keep

    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runKvLatencyTest disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval3_w1c_"$prefix"_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    #------------------------------------------------------------
    # Create with nval1, and rerun latency tests
    #------------------------------------------------------------

    riaktest ts_setup_kv_nval1 --keep

    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runKvLatencyTest disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval1_"$prefix"_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    #------------------------------------------------------------
    # Create with nval3, bitcask
    #------------------------------------------------------------

    riaktest ts_setup_kv_nval3_bitcask --keep

    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runKvLatencyTest disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval3_bitcask_"$prefix"_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    #------------------------------------------------------------
    # Create with AAE turned on
    #------------------------------------------------------------

    riaktest ts_setup_kv_nval3_aae --keep

    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runKvLatencyTest disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/kvlatency_nval3_aae_active_"$prefix"_iter$iIter.txt
	iIter=$[$iIter+1]
    done
}

#-----------------------------------------------------------------------
# Run a chunked-up series of KV latency test sequences.  In each
# chunk, the entire test sequence is run for 'iter' iterations at each
# step, and the sequence is run 'chunk' times
#-----------------------------------------------------------------------

kvLatencyTestSequenceChunk()
{
    nIter=$(simpleValOrDef iter '1' $@)
    startChunk=$(simpleValOrDef start '0' $@)
    nChunk=$(simpleValOrDef chunk '5' $@)
    prefix=$(simpleValOrDef prefix 'kv' $@)

    iChunk=$startChunk
    while [ $iChunk -lt $nChunk ]
    do
	start=$[$nIter * $iChunk]
	echo "Calling seq with iter=$nIter start=$start"
	kvLatencyTestSequence iter=$nIter start=$start prefix=$prefix
	iChunk=$[$iChunk+1]
    done
}


runTsInsertLatencyTest()
{
    disp=$(valOrDef disp 'false' "$@")
    disp=${disp//\"/}

    args=$(valOrDef args '1' "$@")
    args=${args//\"/}

    runerl mod=riak_prof_tests fn=runTsInsertLatencyTests args="$args"
    
    if [ $disp == "true" ]; then
	parseTsPutLatencyTestProfilerOutput `getlast /tmp/client_profiler_results`
    fi
}

runKvLatencyTestSequence()
{
    kvLatencyTestSequence iter=5 start=20 prefix="2.1.4"
    kvLatencyTestSequence iter=5 start=25 prefix="2.1.4"
    kvLatencyTestSequence iter=5 start=30 prefix="2.1.4"
    kvLatencyTestSequence iter=5 start=35 prefix="2.1.4"
}

tsInsertLatencyTestSequence()
{
    nIter=$(simpleValOrDef iter '1' $@)
    startIter=$(simpleValOrDef start '0' $@)

    endIter=$[$startIter + $nIter]

    echo $startIter, $endIter, $nIter

    #------------------------------------------------------------
    # Make the output directory if it doesn't already exist
    #------------------------------------------------------------
    
    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi
    
    #------------------------------------------------------------
    # Set up TS1.4 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------
    
    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.4.0rc2 riak_ee
    rerun target=locked-all script=ts_setup_gen args=--keep nodes=1
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsInsertLatencyTest args="1 false" disp=false
	echo "Copying last profiler output to $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_int_iter$iIter.txt"
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_int_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsInsertLatencyTest args="1 true" disp=false
	echo "Copying last profiler output to $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_date_iter$iIter.txt"
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_date_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_int_iter*.txt`" "`echo $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_date_iter*.txt`" $'TS1.4 Insert Latency\n(integer time)' $'TS1.4 Insert Latency\n(string time)' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False
}

makeInsertCompPlot()
{
#    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_int_iter[0,2,4,6,8].txt`" "`echo $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_int_iter[1,3,5,7,9].txt`" $'TS1.4 Insert Latency\n(integer time)' $'TS1.4 Insert Latency\n(string time)' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False

#        python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_date_iter[0,2,4,6,8].txt`" "`echo $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_date_iter[1,3,5,7,9].txt`" $'TS1.4 Insert Latency\n(integer time)' $'TS1.4 Insert Latency\n(string time)' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False

    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_int_iter*.txt`" "`echo $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_date_iter*.txt`" $'TS1.4 Insert Latency\n(integer time)' $'TS1.4 Insert Latency\n(string time)' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False 'tsinsertcomp_ts1.4_intvdate.png'

    cp tsinsertcomp_ts1.4_intvdate.png $RIAK_TEST_BASE/images
}

makeInsertPutCompPlot()
{
    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.4_1row_iter*.txt`" "`echo $RIAK_TEST_BASE/data/tsinsertlatency_ts1.4_1row_int_iter*.txt`" $'TS1.4 PUT Latency' $'TS1.4 INSERT Latency' $'Latency Ratio (INSERT/PUT)' $'Ratio' overplot=False op='/'
}

runTsPutLatencyTest()
{
    disp=$(valOrDef disp 'false' "$@")
    disp=${disp//\"/}

    runerl mod=riak_prof_tests fn=runTsPutLatencyTests args=$1
    
    if [ $disp == "true" ]; then
	parseTsPutLatencyTestProfilerOutput `getlast /tmp/client_profiler_results`
    fi
}

tsPutLatencyTestSequenceTs1.3VsTs1.4()
{
    nIter=$(simpleValOrDef iter '1' $@)
    startIter=$(simpleValOrDef start '0' $@)

    echo $nIter, $startIter

    #------------------------------------------------------------
    # Make the output directory if it doesn't already exist
    #------------------------------------------------------------
    
    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi
    
    #------------------------------------------------------------
    # Set up TS1.3 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------
    
    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.3.0 riak_ee
    rerun target=locked-all script=ts_setup_gen args=--keep nodes=1
    
    iIter=$startIter
    while [ $iIter -lt $nIter ]
    do
	runTsPutLatencyTest 1 disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row_iter$iIter.txt
	iIter=$[$iIter+1]
    done
	
    #------------------------------------------------------------
    # Set up TS1.4 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------
    
    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.4.0rc2 riak_ee
    iIter=$[$iIter+1]
    
    rerun target=locked-all script=ts_setup_gen args=--keep nodes=1
    
    iIter=$startIter
    while [ $iIter -lt $nIter ]
    do
	runTsPutLatencyTest 1 disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.4_1row_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    #------------------------------------------------------------
    # Finally, run TS1.3 again, in case the baseline is changing
    # slowly with time
    #------------------------------------------------------------

    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.3.0 riak_ee
    rerun target=locked-all script=ts_setup_gen args=--keep nodes=1

    nIter=$[2*$nIter]
    while [ $iIter -lt $nIter ]
    do
	runTsPutLatencyTest 1 disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.4_1row*.txt`" $'TS1.3 Put Latency' $'TS1.4 Put Latency' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False
}

tsPutLatencyTestSequenceW()
{
    nIter=$(simpleValOrDef iter '1' $@)
    startIter=$(simpleValOrDef start '0' $@)

    endIter=$[$startIter + $nIter]

    echo $startIter, $endIter, $nIter

    #------------------------------------------------------------
    # Make the output directory if it doesn't already exist
    #------------------------------------------------------------
    
    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi
    
    #------------------------------------------------------------
    # Set up TS1.3 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------
    
    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.3.0 riak_ee

    rerun target=locked-all script=ts_setup_gen_nval3_w1 args=--keep nodes=3
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsPutLatencyTest "1" disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_w1_iter$iIter.txt
	iIter=$[$iIter+1]
    done
	
    riaktest ts_setup_gen_nval3_def --keep
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsPutLatencyTest "1" disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_def_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    riaktest ts_setup_gen_nval3_w3 --keep
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsPutLatencyTest "1" disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_w3_iter$iIter.txt
	iIter=$[$iIter+1]
    done

}

tsPutLatencyTestSequenceNval3VsNval1()
{
    nIter=$(simpleValOrDef iter '1' $@)
    startIter=$(simpleValOrDef start '0' $@)

    endIter=$[$startIter + $nIter]

    echo $startIter, $endIter, $nIter

    #------------------------------------------------------------
    # Make the output directory if it doesn't already exist
    #------------------------------------------------------------
    
    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi
    
    #------------------------------------------------------------
    # Set up TS1.3 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------
    
    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.3.0 riak_ee

    rerun target=locked-all script=ts_setup_gen_nval3_def args=--keep nodes=3
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsPutLatencyTest "1" disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_iter$iIter.txt
	iIter=$[$iIter+1]
    done
	
    riaktest ts_setup_gen_nval1_def --keep
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsPutLatencyTest "1" disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval1_def_iter$iIter.txt
	iIter=$[$iIter+1]
    done

}

tsPutLatencyTestSequenceExpiryOnVsOff()
{
    nIter=$(simpleValOrDef iter '1' $@)
    startIter=$(simpleValOrDef start '0' $@)

    endIter=$[$startIter + $nIter]

    echo $startIter, $endIter, $nIter

    #------------------------------------------------------------
    # Make the output directory if it doesn't already exist
    #------------------------------------------------------------
    
    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi
    
    #------------------------------------------------------------
    # Set up TS1.3 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------
    
    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.4.0rc7 riak_ee

    rerun target=locked-all script=ts_setup_expiry_off args=--keep nodes=3
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsPutLatencyTest "1" disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.4_expiry_off_iter$iIter.txt
	iIter=$[$iIter+1]
    done
	
    riaktest ts_setup_expiry_on --keep
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsPutLatencyTest "1" disp=false
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsputlatency_ts1.4_expiry_on_iter$iIter.txt
	iIter=$[$iIter+1]
    done
}

makeExpiryPutCompPlot()
{
    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.4_expiry_off_iter*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.4_expiry_on_iter*.txt`" $'TS1.4 Put Latency\nexpiry off' $'TS1.4 Put Latency\nexpiry on' $'$\Delta$ Put Latency (on - off)' $'$\Delta$Latency (%)' overplot=False figfile='tsputcomp_ts1.4_expiry.png'

    cp tsputcomp_ts1.4_expiry.png $RIAK_TEST_BASE/images
}

makeTSPlot()
{
python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval1_def_iter*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_iter*.txt`" $'TS1.3 Put Latency\nnval=1 (split)' $'TS1.3 Put Latency\nval=3' $'$\Delta$ Put Latency (nval=3 - nval=1)' $'$\Delta$Latency (%)' True
}

makeTSPlot2()
{
python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tslatency_2d_nval1.txt`" "`echo $RIAK_TEST_BASE/data/tslatency_2d_nval3.txt`" $'TS1.3 Put Latency\nnval=1 (split)' $'TS1.3 Put Latency\nval=3' $'$\Delta$ Put Latency (nval=3 - nval=1)' $'$\Delta$Latency (%)' False
}

makeYlPlot()
{
    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_w1_iter[0,2,4,6,8,10,12,14,16,18,20].txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_w1_iter[1,3,5,7,9,11,13,15,17,19].txt`" $'TS1.3 Put Latency\nw=1 (split)' $'TS1.3 Put Latency\nw=1 (split)' $'$\Delta$ Put Latency (w=1 - w=1)' $'$\Delta$Latency (%)' False

    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_w1*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_def*.txt`" $'TS1.3 Put Latency\nw=1' $'TS1.3 Put Latency\nw=def (w=2)' $'$\Delta$ Put Latency (w=2 - w=1) ' $'$\Delta$Latency (%)' False

    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_w1*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_w3*.txt`" $'TS1.3 Put Latency\nw=1' $'TS1.3 Put Latency\nw=3' $'$\Delta$ Put Latency (w=3 - w=1)' $'$\Delta$Latency (%)' False

#    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_def*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_nval3_w3*.txt`" $'TS1.3 Put Latency\nw=1' $'TS1.3 Put Latency\nw=3' $'$\Delta$ Put Latency (w=3 - w=2)' $'$\Delta$Latency (%)' False
}

bashTest()
{
    nIter=10
    echo "$nIter"

    nIter=$[2*$nIter]
    echo "$nIter"

    testIter=$[$nIter + $nIter]
    echo "$testIter"
}

makePutCompPlot()
{
#    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row_iter[0-25].txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row_iter[26-50].txt`" $'TS1.3 Put Latency' $'TS1.4 Put Latency' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False

    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row_iter*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.4_1row_iter*.txt`" $'TS1.3 Put Latency' $'TS1.4 Put Latency' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False 'tsputcomp_ts1.3v1.4.png'

    cp tsputcomp_ts1.3v1.4.png $RIAK_TEST_BASE/images

#    python $RIAK_TEST_BASE/python_scripts/tsputtest.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row_iter*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.4_1row_iter*.txt`" $'TS1.3 Put Latency' $'TS1.4 Put Latency' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False

#    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row*[0-3]*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.4_1row*[4-7]*.txt`" $'TS1.3 Put Latency' $'TS1.4 Put Latency' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False

#    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row*[0-4]*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row*[5-9]*.txt`" $'TS1.3 Put Latency' $'TS1.4 Put Latency' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False

#    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row*[0-4]*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.4_1row*[5-9]*.txt`" $'TS1.3 Put Latency' $'TS1.4 Put Latency' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False

#    python $RIAK_TEST_BASE/python_scripts/tsputlatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.3_1row*.txt`" "`echo $RIAK_TEST_BASE/data/tsputlatency_ts1.4_1row*.txt`" $'TS1.3 Put Latency' $'TS1.4 Put Latency' $'$\Delta$ Put Latency' $'$\Delta$Latency (%)' False
}

makeQueryCompPlot()
{
    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.3_1000bytespercol*[0-20]*.txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_1000bytespercol*[0-20]*.txt`" $'TS1.3 Query Latency\n(1000 bytes per column)' $'TS1.4 Query Latency\n(1000 bytes per column)' $'$\Delta$ Query Latency' $'$\Delta$Latency (%)' False 'tsquerycomp_ts1.3v1.4.png'

    cp tsquerycomp_ts1.3v1.4.png $RIAK_TEST_BASE/images

#    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_1000bytespercol*[10-20]*.txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_1000bytespercol*[3-4]*.txt`" $'TS1.3 Query Latency\n(1000 bytes per column)' $'TS1.4 Query Latency\n(1000 bytes per column)' $'$\Delta$ Query Latency' $'$\Delta$Latency (%)' False

#    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.3_1000bytespercol*.txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_1000bytespercol*.txt`" $'TS1.3 Query Latency\n(1000 bytes per column)' $'TS1.4 Query Latency\n(1000 bytes per column)' $'$\Delta$ Query Latency' $'$\Delta$Latency (%)' False
}

parseTsPutLatencyTestProfilerOutput()
{
    fileName=$1

    local pycomm+="import numpy as np\n"
    pycomm+="import matplotlib.pyplot as plt\n"
    pycomm+="import scipy.interpolate as int;\n"
    pycomm+="from mpl_toolkits.mplot3d import Axes3D;\n"
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
    pycomm+="        labelDict[label]['usec']  = np.float(usec[i+1])\n"
    pycomm+="        labelDict[label]['count'] = np.int(counts[i+1])\n"
    pycomm+="  return labelDict\n"
    pycomm+="\n"
    pycomm+="def getProfilerOutput(fileName):\n"
    pycomm+="  d = parseProfilerOutput(fileName, {})\n"
    pycomm+="  pbytes = []\n"
    pycomm+="  pcols  = []\n"
    pycomm+="  putus  = []\n"
    pycomm+="  for key in d.keys():\n"
    pycomm+="    s = key.split('_')\n"
    pycomm+="    if s[0] == 'put':\n"
    pycomm+="      pcols.append(float(s[1]))\n"
    pycomm+="      pbytes.append(np.log10(float(s[2])))\n"
    pycomm+="      ntrial = np.int(s[3])\n"
    pycomm+="      putus.append(np.log10(d[key]['usec']/ntrial))\n"
    pycomm+="\n"
    pycomm+="  x,y,z = getGriddedData(pcols, pbytes, putus)\n"
    pycomm+="  return x,y,z\n"
    pycomm+="\n"
    pycomm+="def getGriddedData(x,y,d):\n"
    pycomm+="  npoints=np.size(x);\n"
    pycomm+="  points = np.ndarray((npoints, 2), np.double);\n"
    pycomm+="\n"
    pycomm+="  for i in range(0,npoints):\n"
    pycomm+="    points[i][0] = x[i];\n"
    pycomm+="    points[i][1] = y[i];\n"
    pycomm+="\n"
    pycomm+="  ux = np.unique(x);\n"
    pycomm+="  uy = np.unique(y);\n"
    pycomm+="\n"
    pycomm+="  x1=np.linspace(np.min(ux), np.max(ux), 200);\n"
    pycomm+="  y1=np.linspace(np.min(uy), np.max(uy), 200);\n"
    pycomm+="  x2,y2 = np.meshgrid(x1, y1);\n"
    pycomm+="  z2=int.griddata(points, d, (x2, y2), method='cubic');\n"
    pycomm+="  return x2, y2, z2\n"
    pycomm+="\n"
    pycomm+="def retick(ax, axname):\n"
    pycomm+="  if axname == 'x':\n"
    pycomm+="    rng = ax.get_xlim()\n"
    pycomm+="  elif axname == 'y':\n"
    pycomm+="    rng = ax.get_ylim()\n"
    pycomm+="  else:\n"
    pycomm+="    rng = ax.get_zlim()\n"
    pycomm+="\n"
    pycomm+="  mn = np.int(np.floor(rng[0]))\n"
    pycomm+="  mx = np.int(np.ceil(rng[1]))\n"
    pycomm+="  ticks = []\n"
    pycomm+="  ticklabels = []\n"
    pycomm+="  for i in range(mn, mx):\n"
    pycomm+="    ticks.append(np.float(i))\n"
    pycomm+="    ticklabels.append('\$10^{' + (\"%%d\" %% i) + '}\$')\n"
    pycomm+="\n"
    pycomm+="  if axname == 'x':\n"
    pycomm+="    ax.set_xticks(ticks)\n"
    pycomm+="    ax.set_xticklabels(ticklabels)\n"
    pycomm+="  elif axname == 'y':\n"
    pycomm+="    ax.set_yticks(ticks)\n"
    pycomm+="    ax.set_yticklabels(ticklabels)\n"
    pycomm+="  else:\n"
    pycomm+="    ax.set_zticks(ticks)\n"
    pycomm+="    ax.set_zticklabels(ticklabels)\n"
    pycomm+="\n"
    pycomm+="  return\n"
    pycomm+="\n"
    pycomm+="def makePlot(x, y, z, Color):\n"
    pycomm+="  fig = plt.figure()\n"
    pycomm+="  fig.set_facecolor('white');\n"
    pycomm+="  ax = fig.add_subplot(1,1,1, projection='3d')\n"
    pycomm+="  ax.plot_surface(x, y, z, color=Color);\n"
    pycomm+="  ax.set_title('TS Put Latency')\n"
    pycomm+="  ax.set_zlabel('Latency (\$\mu\$sec)')\n"
    pycomm+="  ax.set_xlabel('Columns')\n"
    pycomm+="  ax.set_ylabel('Bytes per column)')\n"
    pycomm+="\n"
    pycomm+="  retick(ax, 'y')\n"
    pycomm+="  retick(ax, 'z')\n"
    pycomm+="  plt.show()\n"
    pycomm+="\n"
    pycomm+="x, y, z = getProfilerOutput('$fileName')\n"
    pycomm+="makePlot(x, y, z, 'b')\n"

    printf "$pycomm" > /tmp/pytest.py
    printf "$pycomm" | python
}

runTsSplitsTest()
{
    disp=$(valOrDef disp 'false' "$@")
    disp=${disp//\"/}

    runerl mod=riak_prof_tests fn=runTsSplits args=$1

    if [ $disp == "true" ]; then
	parseTsSplitsProfilerOutput `getlast /tmp/client_profiler_results`
    fi
}

parseTsSplitsProfilerOutput()
{
    fileName=$1

    local pycomm+="import numpy as np\n"
    pycomm+="import matplotlib.pyplot as plt\n"
    pycomm+="import scipy.interpolate as int;\n"
    pycomm+="from mpl_toolkits.mplot3d import Axes3D;\n"
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
    pycomm+="        labelDict[label]['usec']  = np.float(usec[i+1])\n"
    pycomm+="        labelDict[label]['count'] = np.int(counts[i+1])\n"
    pycomm+="  return labelDict\n"
    pycomm+="\n"
    pycomm+="def getProfilerOutput(fileName):\n"
    pycomm+="  d = parseProfilerOutput(fileName, {})\n"
    pycomm+="  spcols = getPutNcols(d)\n"
    pycomm+="\n"
    pycomm+="  fig = plt.figure()\n"
    pycomm+="  fig.set_facecolor('white');\n"
    pycomm+="  ax = fig.add_subplot(1,1,1)\n"
    pycomm+="\n"
    pycomm+="  ncols  = np.size(spcols)\n"
    pycomm+="  legend = []\n"
    pycomm+="  for i in range(0, ncols):\n"
    pycomm+="    x,y = getColData(d, spcols[i])\n"
    pycomm+="    legend.append('Ncol = ' + str(spcols[i]))\n"
    pycomm+="    plt.plot(x,y)\n"
    pycomm+="    plt.hold(True)\n"
    pycomm+="\n"
    pycomm+="  ax.set_xscale('log')\n"
    pycomm+="  ax.set_yscale('log')\n"
    pycomm+="  plt.xlabel('Total bytes per write')\n"
    pycomm+="  plt.ylabel('Put Latency (\$\mu\$sec)')\n"
    pycomm+="  plt.legend(legend, loc='upper left')\n"
    pycomm+="  plt.show()\n"
    pycomm+="  return\n"
    pycomm+="\n"
    pycomm+="def getPutNcols(d):\n"
    pycomm+="  pcols = []\n"
    pycomm+="  for key in d.keys():\n"
    pycomm+="    s = key.split('_')\n"
    pycomm+="    if s[0] == 'put':\n"
    pycomm+="      pcols.append(float(s[1]))\n"
    pycomm+="\n"
    pycomm+="  up = np.unique(pcols)\n"
    pycomm+="  ps = np.argsort(up)\n"
    pycomm+="  ncols  = np.size(up)\n"
    pycomm+="  spcols = np.ndarray(ncols, np.integer)\n"
    pycomm+="\n"
    pycomm+="  for i in range(0, ncols):\n"
    pycomm+="    spcols[i] = up[ps[i]]\n"
    pycomm+="\n"
    pycomm+="  return spcols\n"
    pycomm+="\n"
    pycomm+="def getColData(d, cols):\n"
    pycomm+="  x = []\n"
    pycomm+="  y = []\n"
    pycomm+="  for key in d.keys():\n"
    pycomm+="    s = key.split('_')\n"
    pycomm+="    if s[0] == 'put':\n"
    pycomm+="      ncol = float(s[1])\n"
    pycomm+="      if ncol == cols:\n"
    pycomm+="        ntrial = np.int(s[3])\n"
    pycomm+="        usec = d[key]['usec']/ntrial\n"
    pycomm+="        totalbytes  = np.float(s[2]) * ncol\n"
    pycomm+="        print 'Found ncol = ' + str(ncol) + ' ntrial = ' + str(ntrial) + ' usec/trial = ' + str(usec) + ' bytes/col = ' + s[2] + ' total bytes = ' + str(totalbytes)\n"
    pycomm+="        x.append(totalbytes)\n"
    pycomm+="        y.append(usec)\n"
    pycomm+="\n"
    pycomm+="  inds  = np.argsort(x)\n"
    pycomm+="  ndata = np.size(x)\n"
    pycomm+="  rx = np.ndarray(ndata, np.float)\n"
    pycomm+="  ry = np.ndarray(ndata, np.float)\n"
    pycomm+="\n"
    pycomm+="  for i in range(0, ndata):\n"
    pycomm+="    rx[i] = x[inds[i]]\n"
    pycomm+="    ry[i] = y[inds[i]]\n"
    pycomm+="\n"
    pycomm+="  return rx, ry\n"
    pycomm+="\n"
    pycomm+="getProfilerOutput('$fileName')\n"

    printf "$pycomm" > /tmp/pytest.py
    printf "$pycomm" | python
}

tsQueryLatencyTestSequenceTs1.3VsTs1.4()
{
    nIter=$(simpleValOrDef iter '1' $@)
    startIter=$(simpleValOrDef start '0' $@)

    endIter=$[$startIter + $nIter]

    echo $startIter, $endIter, $nIter

    #------------------------------------------------------------
    # Make the output directory if it doesn't already exist
    #------------------------------------------------------------
    
    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi
    
    #------------------------------------------------------------
    # Set up TS1.3 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------

    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.3.0 riak_ee
    
    rerun target=locked-all script=ts_setup_gen args=--keep nodes=1

    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsQueryLatencyTest disp=false args="1000 all none"
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsquerylatency_ts1.3_1000bytespercol_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    #------------------------------------------------------------
    # Set up TS1.4 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------

    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.4.0rc2 riak_ee
    
    rerun target=locked-all script=ts_setup_gen args=--keep nodes=1

    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsQueryLatencyTest disp=false args="1000 all none"
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_1000bytespercol_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    #------------------------------------------------------------
    # Set up TS1.3 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------

#    \rm riak_ee
#    ln -s branches/riak_ee_riak_ts_ee_1.3.0 riak_ee
#    
#    rerun target=locked-all script=ts_setup_gen args=--keep nodes=1
#
#    nIter=$[2*$nIter]
#    while [ $iIter -lt $nIter ]
#    do
#	runTsQueryLatencyTest disp=false args="1000 all none"
#	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsquerylatency_ts1.3_1000bytespercol_iter$iIter.txt
#	iIter=$[$iIter+1]
 #   done

    #------------------------------------------------------------
    # Make some plots
    #------------------------------------------------------------
    
    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.3_1bytepercol*.txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.3_1000bytespercol*.txt`" $'TS Query Latency\n(1 byte per column)' $'TS Query Latency\n(1000 bytes per column)' $'TS Query Latency\n(1 byte vs 1000 bytes per column)' $'Latency ($\mu$sec)'

    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_1bytepercol*.txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_1000bytespercol*.txt`" $'TS Query Latency\n(1 byte per column)' $'TS Query Latency\n(1000 bytes per column)' $'TS Query Latency\n(1 byte vs 1000 bytes per column)' $'Latency ($\mu$sec)'

    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.3_1000bytespercol*.txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_1000bytespercol*.txt`" $'TS1.3 Query Latency\n(1000 bytes per column)' $'TS1.4 Query Latency\n(1000 bytes per column)' $'$\Delta$ Query Latency' $'$\Delta$Latency (%)' False
}


tsQueryLatencyTestSequenceGroupBy()
{
    nIter=$(simpleValOrDef iter '1' $@)
    echo $nIter
    
    #------------------------------------------------------------
    # Make the output directory if it doesn't already exist
    #------------------------------------------------------------
    
    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi
    
    #------------------------------------------------------------
    # Set up TS1.4 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------

    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.4.0rc2 riak_ee
    
    rerun target=locked-all script=ts_setup_gen args=--keep nodes=1

    iIter="0"
    while [ $iIter -lt $nIter ]
    do
	runTsQueryLatencyTest disp=false args="1 time none"
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_nogroup_iter$iIter.txt
	iIter=$[$iIter+1]
    done
    
    iIter="0"
    while [ $iIter -lt $nIter ]
    do
	runTsQueryLatencyTest disp=false args="1 time time"
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_group_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    #------------------------------------------------------------
    # Make some plots
    #------------------------------------------------------------
    
    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_nogroup*txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_group*.txt`" $'TS Query Latency\n(1 byte per column, no group by)' $'TS Query Latency\n(1 byte per column, group by)' $'$\Delta$ Query Latency' $'$\Delta$Latency (%)' False
}

makeGroupByCompPlot()
{
#    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_nogroup*[0,2,4,6,8]*.txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_nogroup*[1,3,5,7,9]*.txt`" $'TS Query Latency\n(1 byte per column, no group by)' $'TS Query Latency\n(1 byte per column, group by)' $'$\Delta$ Query Latency' $'$\Delta$Latency (%)' False

#    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_group*[0,2,4,6,8]*.txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_group*[1,3,5,7,9]*.txt`" $'TS Query Latency\n(1 byte per column, no group by)' $'TS Query Latency\n(1 byte per column, group by)' $'$\Delta$ Query Latency' $'$\Delta$Latency (%)' False

    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_nogroup*txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_group*.txt`" $'TS Query Latency\n(1 byte per column, no group by)' $'TS Query Latency\n(1 byte per column, group by)' $'$\Delta$ Query Latency' $'$\Delta$Latency (%)' overplot=False figfile='tsquerycomp_ts1.4_groupby.png'

    cp 'tsquerycomp_ts1.4_groupby.png' $RIAK_TEST_BASE/images
}

tsQueryLatencyTestSequenceExpiryOnVsOff()
{
    nIter=$(simpleValOrDef iter '1' $@)
    startIter=$(simpleValOrDef start '0' $@)

    endIter=$[$startIter + $nIter]

    echo $startIter, $endIter, $nIter

    #------------------------------------------------------------
    # Make the output directory if it doesn't already exist
    #------------------------------------------------------------
    
    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi
    
    #------------------------------------------------------------
    # Set up TS1.3 devrel for 1-node cluster, and run the riak_test
    # script to create the cluster
    #------------------------------------------------------------
    
    \rm riak_ee
    ln -s branches/riak_ee_riak_ts_ee_1.4.0rc7 riak_ee

    rerun target=locked-all script=ts_setup_expiry_off args=--keep nodes=3
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsQueryLatencyTest disp=false args="1 time none"
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_expiry_off_iter$iIter.txt
	iIter=$[$iIter+1]
    done
	
    riaktest ts_setup_expiry_on --keep
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	runTsQueryLatencyTest disp=false args="1 time none"
	cp `getlast /tmp/client_profiler_results` $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_expiry_on_iter$iIter.txt
	iIter=$[$iIter+1]
    done

    makeExpiryQueryCompPlot
}

makeExpiryQueryCompPlot()
{
    python $RIAK_TEST_BASE/python_scripts/tsquerylatency_cmp.py "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_expiry_off_iter*.txt`" "`echo $RIAK_TEST_BASE/data/tsquerylatency_ts1.4_expiry_on_iter*.txt`" $'TS Query Latency\n(1 byte per column, expiry off)' $'TS Query Latency\n(1 byte per column, expiry on)' $'$\Delta$ Query Latency' $'$\Delta$Latency (%)' overplot=False figfile='tsquerycomp_ts1.4_expiry.png'

    cp tsquerycomp_ts1.4_expiry.png $RIAK_TEST_BASE/images
}

runTsQueryLatencyTest()
{
    disp=$(valOrDef disp 'false' "$@")
    disp=${disp//\"/}

    args=$(valOrDef args '1' "$@")
    args=${args//\"/}

    runerl mod=riak_prof_tests fn=runTsQueryLatencyTests args="$args"
    
    if [ $disp == "true" ]; then
	parseTsQueryLatencyTestProfilerOutput `getlast /tmp/client_profiler_results`
    fi
}

parseTsQueryLatencyTestProfilerOutput()
{
    fileName=$1

    local pycomm+="import numpy as np\n"
    pycomm+="import matplotlib.pyplot as plt\n"
    pycomm+="import scipy.interpolate as int;\n"
    pycomm+="from mpl_toolkits.mplot3d import Axes3D;\n"
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
    pycomm+="        labelDict[label]['usec']  = np.float(usec[i+1])\n"
    pycomm+="        labelDict[label]['count'] = np.int(counts[i+1])\n"
    pycomm+="  return labelDict\n"
    pycomm+="\n"
    pycomm+="def getProfilerOutput(fileName):\n"
    pycomm+="  d = parseProfilerOutput(fileName, {})\n"
    pycomm+="  cols  = []\n"
    pycomm+="  rows  = []\n"
    pycomm+="  us  = []\n"
    pycomm+="  for key in d.keys():\n"
    pycomm+="    s = key.split('_')\n"
    pycomm+="    if s[0] == 'query':\n"
    pycomm+="      cols.append(float(s[1]))\n"
    pycomm+="      bytes = s[2]\n"
    pycomm+="      rows.append(np.log10(float(s[3])))\n"
    pycomm+="      ntrial = np.int(s[4])\n"
    pycomm+="      us.append(np.log10(d[key]['usec']/ntrial))\n"
    pycomm+="\n"
    pycomm+="  x,y,z = getGriddedData(cols, rows, us)\n"
    pycomm+="  return x,y,z,bytes\n"
    pycomm+="\n"
    pycomm+="def getGriddedData(x,y,d):\n"
    pycomm+="  npoints=np.size(x);\n"
    pycomm+="  points = np.ndarray((npoints, 2), np.double);\n"
    pycomm+="\n"
    pycomm+="  for i in range(0,npoints):\n"
    pycomm+="    points[i][0] = x[i];\n"
    pycomm+="    points[i][1] = y[i];\n"
    pycomm+="\n"
    pycomm+="  ux = np.unique(x);\n"
    pycomm+="  uy = np.unique(y);\n"
    pycomm+="\n"
    pycomm+="  x1=np.linspace(np.min(ux), np.max(ux), 200);\n"
    pycomm+="  y1=np.linspace(np.min(uy), np.max(uy), 200);\n"
    pycomm+="  x2,y2 = np.meshgrid(x1, y1);\n"
    pycomm+="  z2=int.griddata(points, d, (x2, y2), method='cubic');\n"
    pycomm+="  return x2, y2, z2\n"
    pycomm+="\n"
    pycomm+="def retick(ax, axname):\n"
    pycomm+="  if axname == 'x':\n"
    pycomm+="    rng = ax.get_xlim()\n"
    pycomm+="  elif axname == 'y':\n"
    pycomm+="    rng = ax.get_ylim()\n"
    pycomm+="  else:\n"
    pycomm+="    rng = ax.get_zlim()\n"
    pycomm+="\n"
    pycomm+="  mn = np.int(np.floor(rng[0]))\n"
    pycomm+="  mx = np.int(np.ceil(rng[1]))\n"
    pycomm+="  ticks = []\n"
    pycomm+="  ticklabels = []\n"
    pycomm+="  for i in range(mn, mx):\n"
    pycomm+="    if np.float(i) >= rng[0]:\n"
    pycomm+="      ticks.append(np.float(i))\n"
    pycomm+="      ticklabels.append('\$10^{' + (\"%%d\" %% i) + '}\$')\n"
    pycomm+="\n"
    pycomm+="  if axname == 'x':\n"
    pycomm+="    ax.set_xticks(ticks)\n"
    pycomm+="    ax.set_xticklabels(ticklabels)\n"
    pycomm+="  elif axname == 'y':\n"
    pycomm+="    ax.set_yticks(ticks)\n"
    pycomm+="    ax.set_yticklabels(ticklabels)\n"
    pycomm+="  else:\n"
    pycomm+="    ax.set_zticks(ticks)\n"
    pycomm+="    ax.set_zticklabels(ticklabels)\n"
    pycomm+="\n"
    pycomm+="  return\n"
    pycomm+="\n"
    pycomm+="def makePlot(x, y, z, Color, bytes):\n"
    pycomm+="  fig = plt.figure()\n"
    pycomm+="  fig.set_facecolor('white');\n"
    pycomm+="  ax = fig.add_subplot(1,1,1, projection='3d')\n"
    pycomm+="  ax.plot_surface(x, y, z, color=Color);\n"
    pycomm+="  if bytes == '1':\n"
    pycomm+="    ax.set_title('TS Query Latency\\\n(' + str(bytes) + ' byte per column)')\n"
    pycomm+="  else:\n"
    pycomm+="    ax.set_title('TS Query Latency\\\n(' + str(bytes) + ' bytes per column)')\n"
    pycomm+="\n"
    pycomm+="  ax.set_zlabel('Latency (\$\mu\$sec)')\n"
    pycomm+="  ax.set_xlabel('Columns')\n"
    pycomm+="  ax.set_ylabel('Rows per query')\n"
    pycomm+="\n"
    pycomm+="  retick(ax, 'y')\n"
    pycomm+="  retick(ax, 'z')\n"
    pycomm+="  plt.show()\n"
    pycomm+="\n"
    pycomm+="x, y, z, bytes = getProfilerOutput('$fileName')\n"
    pycomm+="makePlot(x, y, z, 'b', bytes)\n"

    printf "$pycomm" > /tmp/pytest.py
    printf "$pycomm" | python
}

makeCompPlots()
{
    makePutCompPlot
    makeQueryCompPlot
    makeInsertCompPlot
    makeGroupByCompPlot
    makeExpiryPutCompPlot
}

makeringplot()
{
    local pycomm="from mpl_toolkits.mplot3d import Axes3D\n"
    pycomm+="import matplotlib.pyplot as plt\n"
    pycomm+="import matplotlib\n"
    pycomm+="import numpy as np\n"
    pycomm+="\n"
    pycomm+="def plot2DRing(ax, ringSize, radius, dr, ringSegments):\n"
    pycomm+="  nPoint = 100\n"
    pycomm+="  dTheta = (2*np.pi)/(nPoint-1)\n"
    pycomm+="  theta = np.ndarray(nPoint)\n"
    pycomm+="  for iRing in range(0, nPoint):\n"
    pycomm+="    theta[iRing] = iRing * dTheta\n"
    pycomm+="\n"
    pycomm+="    x1 = (radius*0.9) * np.cos(theta)\n"
    pycomm+="    y1 = (radius*0.9) * np.sin(theta)\n"
    pycomm+="\n"
    pycomm+="    x2 = (radius*1.3) * np.cos(theta)\n"
    pycomm+="    y2 = (radius*1.3) * np.sin(theta)\n"
    pycomm+="\n"
    pycomm+="# Draw the ring\n"
    pycomm+="\n"
    pycomm+="  transval = 0.1\n"
    pycomm+="  ax.plot(x1, y1, zs=0, zdir='z', color='b', alpha=transval)\n"
    pycomm+="  plt.hold(True)\n"
    pycomm+="  ax.plot(x2, y2, zs=0, zdir='z', color='b', alpha=transval)\n"
    pycomm+="\n"
    pycomm+="# Draw segments\n"
    pycomm+="\n"
    pycomm+="  dTheta = (2*np.pi)/(ringSize)\n"
    pycomm+="  for iRing in range(0, ringSize):\n"
    pycomm+="    theta = iRing * dTheta\n"
    pycomm+="    x1 = (radius*0.9) * np.cos(theta)\n"
    pycomm+="    x2 = (radius*1.3) * np.cos(theta)\n"
    pycomm+="\n"
    pycomm+="    y1 = (radius*0.9) * np.sin(theta)\n"
    pycomm+="    y2 = (radius*1.3) * np.sin(theta)\n"
    pycomm+="\n"
    pycomm+="    ax.plot([x1, x2], [y1, y2], zs=0, zdir='z', color='b', alpha=transval)\n"
    pycomm+="\n"
    pycomm+="    xt = 1.7*radius * np.cos(theta + dTheta/2)\n"
    pycomm+="    yt = 1.7*radius * np.sin(theta + dTheta/2)\n"
    pycomm+="\n"
    pycomm+="    textdir=(xt,yt,0)\n"
    pycomm+="    textstr =  textstr = ringSegments[iRing]\n"
    pycomm+="    ax.text(xt, yt, 0.0, textstr[:5], zdir=textdir, horizontalalignment='center', alpha=0.3)\n"
    pycomm+="\n"
    pycomm+="def getColorNames():\n"
    pycomm+="  cnames = []\n"
    pycomm+="  for name in matplotlib.colors.cnames:\n"
    pycomm+="    cnames.append(name)\n"
    pycomm+="  return cnames\n"
    pycomm+="\n"
    pycomm+="def plot3DRing(ax, iNode, ringSize, nNode, radius, dw, d):\n"
    pycomm+="\n"
    pycomm+="  cnames = getColorNames()\n"
    pycomm+="  colorName = cnames[iNode]\n"
    pycomm+="\n"
    pycomm+="  dTheta = (2*np.pi)/(ringSize)\n"
    pycomm+="  theta = []\n"
    pycomm+="  dx    = []\n"
    pycomm+="  dy    = []\n"
    pycomm+="  dz    = []\n"
    pycomm+="  xpos  = []\n"
    pycomm+="  ypos  = []\n"
    pycomm+="  zpos  = []\n"
    pycomm+="\n"
    pycomm+="  dev = 'dev' + str(iNode + 1)\n"
    pycomm+="\n"
    pycomm+="  for iRing in range(0, ringSize):\n"
    pycomm+="    if d[dev][iRing] > 0.0:\n"
    pycomm+="      theta = iRing * dTheta + dTheta/2\n"
    pycomm+="      dx.append(dw)\n"
    pycomm+="      dy.append(dw)\n"
    pycomm+="      dz.append(d[dev][iRing])\n"
    pycomm+="      xpos.append(radius * (1.3 - (nNode - iNode) * 0.1) * np.cos(theta))\n"
    pycomm+="      ypos.append(radius * (1.3 - (nNode - iNode) * 0.1) * np.sin(theta))\n"
    pycomm+="      zpos.append(theta * 0.0 + 0.1)\n"
    pycomm+="\n"
    pycomm+="  ax.bar3d(xpos, ypos, zpos, dx, dy, dz, color=colorName, alpha=1.0)\n"
    pycomm+="\n"
    pycomm+="def totals(ringSizes):\n"
    pycomm+="  nNode = np.size(ringSizes.keys())\n"
    pycomm+="  nodeSizes = np.zeros(nNode)\n"
    pycomm+="\n"
    pycomm+="  total = 0.0\n"
    pycomm+="  for iNode in range(0, nNode):\n"
    pycomm+="    name = 'dev' + str(iNode + 1)\n"
    pycomm+="    sizes = ringSizes[name]\n"
    pycomm+="\n"
    pycomm+="    for size in sizes:\n"
    pycomm+="      total += size\n"
    pycomm+="      nodeSizes[iNode] += size\n"
    pycomm+="\n"
    pycomm+="\n"
    pycomm+="  cnames = getColorNames()\n"
    pycomm+="\n"
    pycomm+="  for iNode in range(0, nNode):\n"
    pycomm+="    name = 'dev' + str(iNode + 1)\n"
    pycomm+="    if total > 0.0:\n"
    pycomm+="      label = name + ' ' + (\"%%d\" %% (100*(nodeSizes[iNode]/total))) + '%%'\n"
    pycomm+="    else:\n"
    pycomm+="      label = name + ' 0.00' + '%%'\n"
    pycomm+="    ax.text2D(0.05, 0.95 - iNode*0.05, label, transform=ax.transAxes, color=cnames[iNode])\n"

    echo "Here 1"
    pycomm+="$(getring)"

    pycomm+="\n"
    pycomm+="fig = plt.figure(figsize=(20,10))\n"
    pycomm+="ax = fig.add_subplot(111, projection='3d')\n"
    pycomm+="ax.view_init(elev = 60.0, azim= 0.0)\n"
    pycomm+="\n"
    pycomm+="ringSize = np.size(ringSegments)\n"
    pycomm+="nNode    = np.size(ringSizes.keys())\n"
    pycomm+="\n"
    pycomm+="radius = 100.0\n"
    pycomm+="dw = 3.0\n"
    pycomm+="plot2DRing(ax, ringSize, radius, 10*dw, ringSegments)\n"
    pycomm+="\n"
    pycomm+="for i in range(0,nNode):\n"
    pycomm+="  plot3DRing(ax, i, ringSize, nNode, radius, dw, ringSizes)\n"
    pycomm+="  plt.hold(True)\n"
    pycomm+="\n"
    pycomm+="ax.set_axis_off()\n"
    pycomm+="fig.set_facecolor('white')\n"
    pycomm+="totals(ringSizes)\n"
    pycomm+="plt.show()\n"

    printf "$pycomm" > /tmp/pytest.py
    printf "$pycomm" | python
}

getring()
{
    prefixdir="/Users/eml/rt/riak/current/dev"

    nodedirs=("`ls $prefixdir`")

    local pycomm="ringSizes = {}\n"
    
    for node in $nodedirs
    do
	if [ -d $prefixdir/$node/data/leveldb ]
	then

	    #------------------------------------------------------------
	    # Get the list of hash partitions from disk
	    #------------------------------------------------------------
	    
	    ring=("`ls $prefixdir/$node/data/leveldb`")

	    #------------------------------------------------------------
	    # Now iterate over hash partitions for this node
	    #------------------------------------------------------------
	    
	    str="ringSegments = ["
	    erlstr=""
	    first=true
	    for seg in $ring
	    do
		# Construct a python list of segment hashes
		
		size=`ls -l $prefixdir/$node/data/leveldb/$seg | grep sst | awk '{total += ($5 - 68)}; END {print total}'`
		if [ -z $size ]
		then
		    size="0"
		fi
		
		if [ $first == true ]
		then
		    str+="'$seg'"
		    erlstr+="$prefixdir/$node/data/leveldb/$seg"
		    first=false
		else
		    str+=", '$seg'"
		    erlstr+=" $prefixdir/$node/data/leveldb/$seg"
		fi


	    done
	    str+="]"

	    #------------------------------------------------------------
	    # Now get the number of bytes for each segment, and
	    # construct relevant python strings
	    #------------------------------------------------------------
	    
	    bytes=`runerl mod=riak_prof_tests fn=printLeveldbBytes args="$erlstr"`

#	    echo "Running countLeveldbKeys with erlstr = $erlstr"
	    
	    #	    bytes=`runerl mod=riak_prof_tests fn=countLeveldbKeys args="$erlstr"`

	    sizes="ringSizes['$node'] = ["
	    first=true

	    for size in $bytes
	    do
		if [ $first == true ]
		then
		    sizes+="$size"
		    first=false
		else
		    sizes+=", $size"
		fi
	    done
	    sizes+="]"

	    pycomm+="$sizes\n"

	fi
    done

    pycomm+="$str\n"

    printf "$pycomm"
}

getRingBytes()
{
    node=$1
    prefixdir=$2

    #------------------------------------------------------------
    # Get the list of hash partitions from disk
    #------------------------------------------------------------

    ring=("`ls $prefixdir/data/leveldb`")

    #------------------------------------------------------------
    # Now iterate over hash partitions for this node
    #------------------------------------------------------------

    local erlstr=""
    first=true
    for seg in $ring
    do
	# Construct a python list of segment hashes

	if [ -z $size ]
	then
	    size="0"
	fi

	if [ $first == true ]
	then
	    erlstr+="$prefixdir/data/leveldb/$seg"
	    first=false
	else
	    erlstr+=" $prefixdir/data/leveldb/$seg"
	fi

    done
    #------------------------------------------------------------
    # Now get the number of bytes for each segment, and
    # construct relevant python strings
    #------------------------------------------------------------

    bytes=`runerl mod=riak_prof_tests fn=printLeveldbBytes args="$erlstr" riak=$prefixdir`

    local sizes="ringSizes['$node'] = ["
    first=true

    for size in $bytes
    do
	if [ $first == true ]
	then
	    sizes+="$size"
	    first=false
	else
	    sizes+=", $size"
	fi
    done
    sizes+="]"

    pycomm="$sizes\n"
    printf "$pycomm"
}
