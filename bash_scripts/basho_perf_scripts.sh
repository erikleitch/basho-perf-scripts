#!/bin/bash

#=======================================================================
# The following are functions for scraping Bill's framework logfiles
# and making plots
#=======================================================================

#-----------------------------------------------------------------------
# Parse a string of the form
#
# 'cell_size': 1L, 'threads': 50L, 'columns': 1L
#
# To extract the value of a named parameter
#-----------------------------------------------------------------------

parseparam()
{
    local sub=$1
    param=$2

    sub=`echo "$sub" | tr " " "\0"`

    array=(`echo "$sub" | tr "," " "`)

    for i in "${!array[@]}"	   
    do	
	arr=${array[i]}
	if [[ $arr =~ \'$param\':(.*)L ]]; then
	    echo ${BASH_REMATCH[1]}
	    return
	fi
    done
}

parseparamnew()
{
    local sub=$1
    param=$2

    sub=`echo "$sub" | tr " " "\0"`
    sub=`echo "$sub" | tr "\"" "\0"`
    sub=`echo "$sub" | tr "{" ","`
    sub=`echo "$sub" | tr "}" ","`

    array=(`echo "$sub" | tr "," " "`)

    for i in "${!array[@]}"	   
    do	
	arr=${array[i]}
	if [[ $arr =~ $param':'(.*) ]]; then
	    echo ${BASH_REMATCH[1]}
	    return
	fi
    done
}

getElement()
{
    cellsize=$1
    iter=$2

    if [ ! -z "$cellsize" ]; then
	cellarr=(`echo $cellsize`)
	cellarrsize=${#cellarr[@]}
	if [ $cellarrsize -gt 1 ]; then
	    echo ${cellarr[$iter]//\"/}
	else
	    echo ${cellarr[0]//\"/}
	fi
    else
	echo ""
    fi
}

#-----------------------------------------------------------------------
# Given a throughput sum, and a set of parameters, return writes/sec, or
# empty string if none is available
#-----------------------------------------------------------------------

getWrites()
{
    local av=$1
    local fieldcount=$2
    local batchsize=$3
    local iter=$4

    fieldcount=$(getElement "$fieldcount" $iter)
    batchsize=$(getElement "$batchsize" $iter)

    if [ ! -z "$fieldcount" ] && [ ! -z "$batchsize" ]; then
	echo $av"*"$fieldcount"*"$batchsize | bc
    else
	echo ""
    fi
}

#-----------------------------------------------------------------------
# Given a throughput sum, and a set of parameters, return bytes/sec, or
# empty string if none is available
#-----------------------------------------------------------------------

getBytes()
{
    local av=$1
    local cellsize=$2
    local fieldcount=$3
    local batchsize=$4
    local iter=$5

    cellsize=$(getElement "$cellsize" $iter)
    fieldcount=$(getElement "$fieldcount" $iter)
    batchsize=$(getElement "$batchsize" $iter)
    
    if [ ! -z "$fieldcount" ] && [ ! -z "$cellsize" ] && [ ! -z "$batchsize" ]; then
	echo $av"*"$cellsize"*"$fieldcount"*"$batchsize | bc
    else
	echo ""
    fi
}

#-----------------------------------------------------------------------
# Given a throughput sum, and a set of parameters, write the data to an
# output file
#-----------------------------------------------------------------------

writeVal()
{
    local av=$1
    param1=$2
    val1=$3
    param2=$4
    val2=$5
    cellsize=$6
    iter=$7
    startdate="$8"
    enddate="$9"
    stat=${10}

    echo "Inside writeVal with stat='$stat'"
    
    total=0.0
    if [ $stat == "ops" ]; then
	av+=")"
	total=`echo $av | bc`

	echo "About to getWrites"
	writes=$(getWrites "$av" $param1 $val1 $param2 $val2)
	bytes=$(getBytes "$av" $param1 $val1 $param2 $val2 "$cellsize" $iter)

	echo "Inside writeVal with stat=$stat total=$total writes=$writes"
	if [ $total != "0" ]; then
	    if [ ! -z "$bytes" ]; then
		echo $val1 " " $val2 " " $total " " $writes " " $bytes >> "/tmp/dat"$iter".txt"
	    elif [ ! -z "$writes" ]; then
		echo $val1 " " $val2 " " $total " " $writes >> "/tmp/dat"$iter".txt"
	    else
		echo $val1 " " $val2 " " $total >> "/tmp/dat"$iter".txt"
	    fi
	fi

    elif [ $stat == \"cpu\" ]; then
	total=$(pythonInfluxQuery "$startdate" "$enddate")
	echo $val1 " " $val2 " " $total >> "/tmp/dat"$iter".txt"
    fi
}

writeValNew()
{
    local av=$1
    param1=$2
    val1=$3
    param2=$4
    val2=$5
    cellsize=$6
    fieldcount=$7
    batchsize=$8
    iter=$9
    startdate="${10}"
    enddate="${11}"
    stat=${12}

    echo "Inside writeValNew with stat='$stat' batchsize='$batchsize'"
    
    total=0.0
    if [ $stat == ops ]; then
	av+=")"
	total=`echo $av | bc`

#	echo "About to call getWrites getBytes"
	
	writes=$(getWrites "$av" "$fieldcount" "$batchsize" $iter)
	bytes=$(getBytes "$av" "$cellsize"  "$fieldcount" "$batchsize" $iter)

#	echo "Inside writeValNew with stat=$stat total=$total writes=$writes bytes=$bytes"

#	if [ $total != "0" ]; then
#	    echo "total is not zero"
#	fi
#	
#	if [ ! -z "$bytes" ]; then
#	    echo "Bytes is not empty"
#	fi
	
	if [ $total != "0" ]; then
	    if [ ! -z "$bytes" ]; then
		echo $val1 " " $val2 " " $total " " $writes " " $bytes >> "/tmp/dat"$iter".txt"
	    elif [ ! -z "$writes" ]; then
		echo $val1 " " $val2 " " $total " " $writes >> "/tmp/dat"$iter".txt"
	    else
		echo $val1 " " $val2 " " $total >> "/tmp/dat"$iter".txt"
	    fi
	fi

    elif [ $stat == \"cpu\" ]; then
	total=$(pythonInfluxQuery "$startdate" "$enddate")
	echo $val1 " " $val2 " " $total >> "/tmp/dat"$iter".txt"
    fi
}

#-----------------------------------------------------------------------
# Read through a framework log file, extracting parameters and
# throughputs
#-----------------------------------------------------------------------

getTestData()
{
    local files="$1"

    iIter="0"                                                                      
    for i in $files; do
	getTestDataSingle $i $2 $3 "$4" $iIter $5
        iIter=$[$iIter+1] 
    done
    printf "\n"
}

getTestDataYcsb()
{
    local files=$(valOrDef files '' "$@")
    files=${files//\"/}
    
    local param1=$(valOrDef param1 '' "$@")
    param1=${param1//\"/}
    
    local param2=$(valOrDef param2 '' "$@")
    param2=${param2//\"/}
    
    local cellsize=$(valOrDef cellsize '' "$@")
    
    local stat=$(valOrDef stat 'ops' "$@")
    stat=${stat//\"/}

    local fieldcount=$(valOrDef fieldcount '' "$@")

    local batchsize=$(valOrDef batchsize '' "$@")
    batchsize=${batchsize//\"/}
    
    echo "Inside getTestDataYcsb with files='$files'"
    
    local iIter="0"                                                                      
    for i in $files; do
	getTestDataSingleYcsb file=$i param1=$param1 param2=$param2 stat=$stat fieldcount="$fieldcount" batchsize="$batchsize" cellsize="$cellsize" iter=$iIter 
        iIter=$[$iIter+1] 
    done
    printf "\n"
}

getTestDataSingle()
{
    file=$1
    param1=$2
    param2=$3
    cellsize=$4
    iter=$5
    stat=$6

    printf "\rProcessing $file..."
    
    first="true"
    haveEnd="false"

    echo "Here 1"
	
    while read p; do
	case "$p" in

	    #------------------------------------------------------------
	    # If this is a config line, extract the parameter vals from it
	    #------------------------------------------------------------
	    
	    *config=*)
		case "$p" in
		    *'parent_id=None'*)
		    ;;
		    *)
			if [[ $p =~ [a-z]*{(.*)}[a-z]* ]]; then
			    sub=${BASH_REMATCH[1]}
			    if [[ $sub =~ [a-z]*{(.*)}[a-z]* ]]; then
				sub=${BASH_REMATCH[1]}

				if [ $first == "true" ]; then
				    first="false"
				    \rm "/tmp/dat"$iter".txt"
				else
				    writeVal "$av" $param1 $val1 $param2 $val2 "$cellsize" $iter "$startdate" "$enddate" $stat
				fi
				
				val1=$(parseparam "$sub" $param1)
				val2=$(parseparam "$sub" $param2)

				av="scale=4;(0.0"
			    fi
				
			fi
			;;
		esac
		;;

	    #------------------------------------------------------------
	    # Else accumulate throughputs for this set of parameters
	    # if this is a throughput report
	    #------------------------------------------------------------
	    
	    *deploy_basho_perf*)
		startdate=$(getDate "$p")
		haveEnd="false"
		;;

	    #------------------------------------------------------------
	    # Else accumulate throughputs for this set of parameters
	    # if this is a throughput report
	    #------------------------------------------------------------
	    
	    *\[INFO\]\ Throughput*)
		if [[ $p =~ [a-z]*Throughput:(.*) ]]; then
		    av+="+"${BASH_REMATCH[1]}
		fi

		if [ $haveEnd == "false" ]; then
		    enddate=$(getDate "$p")
		    haveEnd="true"
		fi
		;;
	esac
    done <$file

    writeVal "$av" $param1 $val1 $param2 $val2 "$cellsize" $iter "$startdate" "$enddate" $stat
}

getTestDataSingleNew()
{
    file=$1
    param1=$2
    param2=$3
    cellsize=$4
    iter=$5
    stat=$6
    
    printf "\rProcessing $file..."
    
    first="true"
    haveEnd="false"

    while read p; do
	case "$p" in

	    #------------------------------------------------------------
	    # If this is a config line, extract the parameter vals from it
	    #------------------------------------------------------------
	    
	    *'event_type=start'*)
		if [[ $p =~ [a-z]*{(.*)}[a-z]* ]]; then
		    sub=${BASH_REMATCH[1]}
		    if [[ $sub =~ [a-z]*{(.*)}[a-z]* ]]; then
			sub=${BASH_REMATCH[1]}
			
			if [ $first == "true" ]; then
			    first="false"
			    \rm "/tmp/dat"$iter".txt"
			else
			    writeVal "$av" $param1 $val1 $param2 $val2 "$cellsize" $iter "$startdate" "$enddate" $stat
			fi
			
			val1=$(parseparam "$sub" $param1)
			val2=$(parseparam "$sub" $param2)
			
			av="scale=4;(0.0"
		    fi
		    
		fi
		;;

	    #------------------------------------------------------------
	    # Else accumulate throughputs for this set of parameters
	    # if this is a throughput report
	    #------------------------------------------------------------
	    
	    *deploy_basho_perf*)
		startdate=$(getDate "$p")
		haveEnd="false"
		;;

	    #------------------------------------------------------------
	    # Else accumulate throughputs for this set of parameters
	    # if this is a throughput report
	    #------------------------------------------------------------
	    
	    *\[INFO\]\ Throughput*)
		if [[ $p =~ [a-z]*Throughput:(.*) ]]; then
		    av+="+"${BASH_REMATCH[1]}
		fi

		if [ $haveEnd == "false" ]; then
		    enddate=$(getDate "$p")
		    haveEnd="true"
		fi
		;;
	esac
    done <$file

    writeVal "$av" $param1 $val1 $param2 $val2 "$cellsize" $iter "$startdate" "$enddate" $stat
}

ycsbtest()
{
    getTestDataSingleYcsb output.log threadcount fieldcount 1 0 "ops"
}

getTestDataSingleAnyYcsb()
{
    file=$1

    printf "\rProcessing $file..."
    
    first="true"
    haveEnd="false"

    av="0"
    while read p; do
	case "$p" in

	    #------------------------------------------------------------
	    # If this is a config line, extract the parameter vals from it
	    #------------------------------------------------------------
	    
	    *'event_type="start"'*)
#		echo "Found line: $p"
		if [[ $p =~ [a-z]*{(.*)}[a-z]* ]]; then
		    sub=${BASH_REMATCH[1]}
#		    echo "Found sub: $sub"

		    if [ $first == "true" ]; then
			first="false"
		#	rm "/tmp/dat"$iter".txt"
		    else
			echo "Calling writeVal with val1 = $val1 val2 = $val2" > /dev/null
		    fi
		    
		    total=`echo $av | bc`
		    echo "Total = $total"
		    
		    av="0"
		fi
		;;

	    #------------------------------------------------------------
	    # Else accumulate throughputs for this set of parameters
	    # if this is a throughput report
	    #------------------------------------------------------------
	    
	    *deploy_basho_perf*)
		startdate=$(getDate "$p")
		haveEnd="false"
		;;

	    #------------------------------------------------------------
	    # Else accumulate throughputs for this set of parameters
	    # if this is a throughput report
	    #------------------------------------------------------------
	    
	    *\[OVERALL\],\ Throughput*)
#		echo "Found a throughput line: $p"
		if [[ $p =~ [a-z]*'Throughput(ops/sec), '(.*) ]]; then
		    av+="+"${BASH_REMATCH[1]}
#		    echo "av = $av"
		fi

		if [ $haveEnd == "false" ]; then
		    enddate=$(getDate "$p")
		    haveEnd="true"
		fi
		;;
	esac
    done <$file

    if [ $av != "0" ]; then
	total=`echo $av | bc`
	echo "Total = $total"
    fi

}

getTestDataSingleYcsb()
{
    local file=$(valOrDef file '' "$@")
    file=${file//\"/}
    
    local param1=$(valOrDef param1 '' "$@")
    param1=${param1//\"/}
    
    local param2=$(valOrDef param2 '' "$@")
    param2=${param2//\"/}
    
    local cellsize=$(valOrDef cellsize '' "$@")
    local fieldcount=$(valOrDef fieldcount '' "$@")
    local batchsize=$(valOrDef batchsize '' "$@")

    local stat=$(valOrDef stat 'ops' "$@")
    stat=${stat//\"/}

    local iter=$(valOrDef iter 'ops' "$@")
    iter=${iter//\"/}

    echo "Inside YCSB with file=$file param1=$param1 param2=$param2 stat='$stat' cellsize=$cellsize fieldcount=$fieldcount batchsize=$batchsize iter=$iter "
    
    printf "\rProcessing $file..."
    
    first="true"
    haveEnd="false"

    while read p; do
	case "$p" in

	    #------------------------------------------------------------
	    # If this is a config line, extract the parameter vals from it
	    #------------------------------------------------------------
	    
	    *'event_type="start"'*)
		echo "Found line: $p"
		if [[ $p =~ [a-z]*{(.*)}[a-z]* ]]; then
		    sub=${BASH_REMATCH[1]}
		    echo "Found sub: $sub"

		    if [ $first == "true" ]; then
			first="false"
			\rm "/tmp/dat"$iter".txt"
		    else
			echo "Calling writeVal with val1 = $val1 val2 = $val2"
			writeValNew "$av" $param1 $val1 $param2 $val2 "$cellsize" "$fieldcount" "$batchsize" $iter "$startdate" "$enddate" $stat
		    fi
		    
		    val1=$(parseparamnew "$sub" $param1)
		    val2=$(parseparamnew "$sub" $param2)

		    # These are all the things we need to calculate writes/sec and bytes/sec
		    
		    nbyte=$(parseparamnew "$sub" fieldlength)
		    ncol=$(parseparamnew "$sub" fieldcount)
		    batch=$(parseparamnew "$sub" batchsize)

		    echo "Found nbyte=$nbyte ncol=$ncol batch=$batch"
		    
		    #------------------------------------------------------------
		    # Override defaults if we can
		    #------------------------------------------------------------
		    
		    if [ ! -z $batch ]
		    then
			batchsize=$batch
		    fi

		    if [ ! -z $nbyte ]
		    then
			cellsize=$nbyte
		    fi

		    if [ ! -z $ncol ]
		    then
			fieldcount=$ncol
		    fi
		    
		    echo "param1 = $param1 val1 = $val1 param2 = $param2 val2 = $val2 nbyte = $nbyte ncol = $ncol batch = $batch"
		    
		    av="scale=4;(0.0"
		fi
		;;

	    #------------------------------------------------------------
	    # Else accumulate throughputs for this set of parameters
	    # if this is a throughput report
	    #------------------------------------------------------------
	    
	    *deploy_basho_perf*)
		startdate=$(getDate "$p")
		haveEnd="false"
		;;

	    #------------------------------------------------------------
	    # Else accumulate throughputs for this set of parameters
	    # if this is a throughput report
	    #------------------------------------------------------------
	    
	    *\[OVERALL\],\ Throughput*)
#		echo "Found a throughput line: $p"
		if [[ $p =~ [a-z]*'Throughput(ops/sec), '(.*) ]]; then
		    av+="+"${BASH_REMATCH[1]}
#		    echo "av = $av"
		fi

		if [ $haveEnd == "false" ]; then
		    enddate=$(getDate "$p")
		    haveEnd="true"
		fi
		;;
	esac
    done <$file

    writeValNew "$av" $param1 $val1 $param2 $val2 "$cellsize" "$fieldcount" "$batchsize" $iter "$startdate" "$enddate" $stat
}

getDate()
{
    line="$1"
    if [[ $p =~ (.*)\ -- ]]; then
	echo ${BASH_REMATCH[1]}
    fi
    
}

#-----------------------------------------------------------------------
# Generate python plots of the data we scraped from the logfile
#-----------------------------------------------------------------------

generatePythonPlots()
{
    files="$1"
    param1=$2
    param2=$3
    overplot=$4
    figsize=$5
    labels="$6"
    title=$7
    scale=$8
    plotwith=$9
    output=${10}
    figview=${11}
    plotwithaction=${12}
    cellsizes=${13}

    local contour=$(valOrDef contour 'none' "$@")
    contour=${contour//\"/}

    echo "contour = '$contour'"
    echo "figsize = $figsize"
    echo "figview = $figview"
    echo "labels = $labels"
    echo "param1 = $param1"
    echo "param2 = $param2"
    echo "overplot = $overplot"
    
    pycomm="import scipy.interpolate as int;\n"
    pycomm+="import numpy as np;\n"
    pycomm+="import matplotlib.pyplot as plt;\n"
    pycomm+="from matplotlib import rcParams;\n"
    pycomm+="from mpl_toolkits.mplot3d import Axes3D;\n"

    pycomm+="defaultFontsize=14\n"

    pycomm+="\n"
    pycomm+="def getScalesAndUnits(fileNames):\n"
    pycomm+="\n"
    pycomm+="  mxs = []\n"
    pycomm+="  iFile=0\n"
    pycomm+="  ncolmax=0\n"
    pycomm+="\n"
    pycomm+="  for file in fileNames:\n"
    pycomm+="    dat = np.loadtxt(file);\n"
    pycomm+="    nline = np.shape(dat)[0];\n"
    pycomm+="    ncol  = np.shape(dat)[1];\n"
    pycomm+="    if ncol > ncolmax:\n"
    pycomm+="      ncolmax = ncol;\n"
    pycomm+="\n"
    pycomm+="  mxs =[]\n"
    pycomm+="  for i in range(2,ncol):\n"
    pycomm+="    mxs.append([])\n"
    pycomm+="\n"
    pycomm+="  for file in fileNames:\n"
    pycomm+="    dat = np.loadtxt(file);\n"
    pycomm+="    nline = np.shape(dat)[0];\n"
    pycomm+="    ncol  = np.shape(dat)[1];\n"
    pycomm+="\n"
    pycomm+="    for i in range(2,ncol):\n"
    pycomm+="      mxs[i-2].append(np.max(dat[0:nline,i]))\n"
    pycomm+="\n"
    pycomm+="  units  = []\n"
    pycomm+="  scales = []\n"
    pycomm+="  maxs = []\n"
    pycomm+="\n"
    pycomm+="  for i in range(2,ncol):\n"
    pycomm+="    index = i-2\n"
    pycomm+="    if np.max(mxs[index]) > 1e9:\n"
    pycomm+="      scales.append(1e9)\n"
    pycomm+="      units.append('G')\n"
    pycomm+="      maxs.append(np.max(mxs[index])/1e9)\n"
    pycomm+="    elif np.max(mxs[index]) > 1e6:\n"
    pycomm+="      scales.append(1e6)\n"
    pycomm+="      units.append('M')\n"
    pycomm+="      maxs.append(np.max(mxs[index])/1e6)\n"
    pycomm+="    elif np.max(mxs[index]) > 1e3:\n"
    pycomm+="      scales.append(1e3)\n"
    pycomm+="      units.append('K')\n"
    pycomm+="      maxs.append(np.max(mxs[index])/1e3)\n"
    pycomm+="    else:\n"
    pycomm+="      scales.append(1)\n"
    pycomm+="      units.append('')\n"
    pycomm+="      maxs.append(np.max(mxs[index]))\n"
    pycomm+="\n"
    pycomm+="  return scales, units, maxs\n"

    pycomm+="\n"
    pycomm+="def getSubplots(fileNames, overplot):\n"
    pycomm+="\n"
    pycomm+="  ncolmax = 0\n"
    pycomm+="  for file in fileNames:\n"
    pycomm+="    dat = np.loadtxt(file);\n"
    pycomm+="    nline = np.shape(dat)[0];\n"
    pycomm+="    ncol  = np.shape(dat)[1];\n"
    pycomm+="    if ncol > ncolmax:\n"
    pycomm+="      ncolmax = ncol;\n"
    pycomm+="\n"
    pycomm+="  axes=[None] * (ncolmax-2)\n"
    pycomm+="\n"
    pycomm+="  nColPerRow=ncolmax-2\n"
    pycomm+="  nFile=np.shape(fileNames)[0];\n"
    pycomm+="\n"
    pycomm+="  if overplot:\n"
    pycomm+="    for iCol in range(0,nColPerRow):\n"
    if [ $contour == contouronly ]; then
	pycomm+="      ax = fig.add_subplot(1, nColPerRow, iCol+1);\n"
    else
	pycomm+="      ax = fig.add_subplot(1, nColPerRow, iCol+1, projection='3d');\n"
    fi
    pycomm+="      axes[iCol] = []\n"
    pycomm+="      for iFile in range(0,nFile):\n"
    pycomm+="        axes[iCol].append(ax)\n"
    pycomm+="  else:\n"
    pycomm+="    for iFile in range(0,nFile):\n"
    pycomm+="      dat  = np.loadtxt(fileNames[iFile]);\n"
    pycomm+="      nCol = np.shape(dat)[1] - 2;\n"
    pycomm+="\n"
    pycomm+="      # Iterate over all columns actually present in this file, adding\n"
    pycomm+="      # a subplot for each one\n"
    pycomm+="\n"
    pycomm+="      for iCol in range(0,nCol):\n"
    pycomm+="        currSubplotInd=nColPerRow * iFile + iCol + 1\n"
    pycomm+="        if axes[iCol] == None:\n"
    pycomm+="          axes[iCol] = []\n"
    if [ $contour == contouronly ]; then
	pycomm+="        axes[iCol].append(fig.add_subplot(nFile, nColPerRow, currSubplotInd));\n"
    else
	pycomm+="        axes[iCol].append(fig.add_subplot(nFile, nColPerRow, currSubplotInd, projection='3d'));\n"
    fi
    pycomm+="\n"
    pycomm+="  return axes\n"

    pycomm+="\n"
    pycomm+="def getData(fileName, index):\n"
    pycomm+="\n"
    pycomm+="  dat = np.loadtxt(fileName);\n"
    pycomm+="  nline = np.shape(dat)[0];\n"
    pycomm+="  x = dat[0:nline,0];\n"
    pycomm+="  nx = np.size(np.unique(x));\n"
    #    pycomm+="  y = np.log10(dat[0:nline,1]);\n"
    pycomm+="  y = dat[0:nline,1];\n"
    pycomm+="  ny = np.size(np.unique(y));\n"
    pycomm+="  [d, unit] = getDataAndUnits(dat, nline, index);\n"
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
    pycomm+="  if nx > 3 and ny > 3:\n"
    pycomm+="    z2=int.griddata(points, d, (x2, y2), method='cubic')\n"
    pycomm+="  else:\n"
    pycomm+="    z2=int.griddata(points, d, (x2, y2), method='linear')\n"
    pycomm+="  return x2, y2, z2, unit\n"
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
    pycomm+="def makeSubPlot(fileName, index, ax, doHold, Color, xlabel, ylabel, zlabel, scale, unit, maxVal):\n"
    pycomm+="\n"
    pycomm+="  x,y,z,unit2 = getData(fileName, index);\n"
    pycomm+="  plt.hold(doHold);\n"

    if [ $contour == contouronly ]; then
	pycomm+="  plt.hold(True);\n"
	pycomm+="  CSZ = ax.contour(x, y, z/scale, 20)\n"
	pycomm+="  extent =  ax.axis()\n"
	pycomm+="  ax.set_aspect(abs((extent[1]-extent[0])/(extent[3]-extent[2])))\n"
	pycomm+="  ax.clabel(CSZ, inline=True, fontsize=10)\n"
	pycomm+="  ax.set_title('\\\\n' + zlabel + ' (' + unit + ')', fontsize=defaultFontsize);\n"
    elif [ $contour == contour ]; then
	pycomm+="  plt.hold(True);\n"
	pycomm+="  ax.plot_surface(x, y, z/scale, color=Color);\n"
	pycomm+="  ax.set_zlabel('\\\\n' + zlabel + ' (' + unit + ')', fontsize=defaultFontsize);\n"
	pycomm+="  ax.set_zlim(0, maxVal*1.1);\n"
	pycomm+="  CSZ = ax.contourf(x, y, z/scale, 20, zdir='z', offset=0)\n"
	pycomm+="  plt.colorbar(CSZ, orientation='horizontal', ax=ax, shrink=0.7)\n"
    else
	pycomm+="  ax.plot_surface(x, y, z/scale, color=Color);\n"
	pycomm+="  ax.set_zlabel('\\\\n' + zlabel + ' (' + unit + ')', fontsize=defaultFontsize);\n"
	pycomm+="  ax.set_zlim(0, maxVal*1.1);\n"
    fi

    pycomm+="  ax.set_xlabel('\\\\n' + xlabel, fontsize=defaultFontsize);\n"
    pycomm+="  ax.set_ylabel('\\\\n' + ylabel, fontsize=defaultFontsize);\n"

    #    pycomm+="  retick(ax, 'y')\n"
    pycomm+="  ax.tick_params(labelsize=defaultFontsize)\n"
    pycomm+="\n"
    pycomm+="def makeSubPlotTwo(fileName, fileName2, index, action, ax, doHold, Color, xlabel, ylabel, zlabel, scale, unit, maxVal):\n"
    pycomm+="\n"
    pycomm+="  x1,y1,z1,unitIgnore = getData(fileName, index);\n"
    pycomm+="  x2,y2,z2,unitIgnore = getData(fileName2, index);\n"
    pycomm+="  plt.hold(doHold);\n"
    pycomm+="  if action == '-':\n"
    pycomm+="    ax.plot_surface(x1, y1, (z1 - z2)/scale, color=Color);\n"
    pycomm+="  elif action == '+':\n"
    pycomm+="    ax.plot_surface(x1, y1, (z1 + z2)/scale, color=Color);\n"
    pycomm+="  elif action == '/':\n"
    pycomm+="\n"
    if [ $contour == contouronly ]; then
	echo "CONTOUR IS contouronly"
	pycomm+="    plt.hold(True);\n"
	pycomm+="\n"
	pycomm+="    r = z1/z2;\n"
	pycomm+="    maxVal = np.max(z1/z2);\n"
	pycomm+="    CSZ = ax.contour(x1, y1, r, 20)\n"
	pycomm+="    extent =  ax.axis()\n"
	pycomm+="    ax.set_aspect(abs((extent[1]-extent[0])/(extent[3]-extent[2])))\n"
	pycomm+="    ax.clabel(CSZ, inline=True, fontsize=10)\n"
	pycomm+="    ax.set_title('\\\\n' + zlabel + ' (ratio)', fontsize=defaultFontsize);\n"
	pycomm+="    print 'Label = ' + zlabel + ' mean = ' + str(np.mean(r)) + ' +- ' + str(np.std(r, ddof=1));\n"
	pycomm+="    print 'Ratio of max = ' + str(np.max(z1)/np.max(z2));\n"
	pycomm+="    print 'Max of ratio = ' + str(np.max(r));\n"

    elif [ $contour == contour ]; then
	echo "CONTOUR IS contour"
	pycomm+="    plt.hold(True);\n"
	pycomm+="\n"
	pycomm+="    r = z1/z2;\n"
	pycomm+="    ax.plot_surface(x1, y1, r, color=Color);\n"
	pycomm+="    maxVal = np.max(z1/z2);\n"
	pycomm+="    CSZ = ax.contourf(x1, y1, r, 20, zdir='z', offset=0)\n"
	pycomm+="    plt.colorbar(CSZ, orientation='horizontal', ax=ax, shrink=0.7)\n"
	pycomm+="    ax.set_zlabel('\\\\n' + zlabel + ' (' + unit + ')');\n"
	pycomm+="    ax.set_zlim(0, maxVal*1.1);\n"
    else
	echo "CONTOUR IS $contour"
	pycomm+="    r = z1/z2;\n"
	pycomm+="    ax.plot_surface(x1, y1, r, color=Color);\n"
	pycomm+="    maxVal = np.max(z1/z2);\n"
	pycomm+="    ax.set_zlabel('\\\\n' + zlabel + ' (' + unit + ')');\n"
	pycomm+="    ax.set_zlim(0, maxVal*1.1);\n"
    fi
    pycomm+="  elif action == '*':\n"
    pycomm+="    ax.plot_surface(x1, y1, z1 * z2, color=Color);\n"
    pycomm+="  ax.set_xlabel('\\\\n' + xlabel);\n"
    pycomm+="  ax.set_ylabel('\\\\n' + ylabel);\n"
    pycomm+="#  retick(ax, 'y')\n"

    echo "view = $figview"
    if [ $figview != \"\" ]; then
	pycomm+="  ax.view_init${figview//\"/};\n"
    fi
    
    pycomm+="\n"

    pycomm+="def plotFiles(files, plotwithfiles, plotwithaction, axes, colors, scales, units, maxs):\n"
    pycomm+="  nfile=np.shape(files)[0]\n"
    pycomm+="  ncolor=np.shape(colors)[0];\n"
    pycomm+="\n"
    pycomm+="  print 'plotwithaction = ' + str(plotwithaction)"
    pycomm+="\n"
    pycomm+="  for iFile in range(0,nfile):\n"
    pycomm+="\n"
    pycomm+="    if plotwithfiles != None:\n"
    pycomm+="      if iFile == 0:\n"
    pycomm+="        if plotwithaction == 'p':\n"
    pycomm+="          plotData(files, iFile, False, axes, 'c', scales, units, maxs)\n"
    pycomm+="          plotData(plotwithfiles, iFile, True, axes, 'm', scales, units, maxs)\n"
    pycomm+="        else:\n"
    pycomm+="          plotDataTwo(files, plotwithfiles, plotwithaction, iFile, False, axes, 'c', scales, units, maxs)\n"
    pycomm+="      else:\n"
    pycomm+="        if plotwithaction == 'p':\n"
    pycomm+="          plotData(files, iFile,  True, axes, 'c', scales, units, maxs)\n"
    pycomm+="          plotData(plotwithfiles, iFile, True, axes, 'm', scales, units, maxs)\n"
    pycomm+="        else:\n"
    pycomm+="          plotDataTwo(files, plotwithfiles, plotwithaction, iFile, True, axes, 'c', scales, units, maxs)\n"
    pycomm+="    else:\n"
    pycomm+="      if iFile == 0:\n"
    pycomm+="        plotData(files, iFile, False, axes, colors[iFile %% ncolor], scales, units, maxs)\n"
    pycomm+="      else:\n"
    pycomm+="        plotData(files, iFile,  True, axes, colors[iFile %% ncolor], scales, units, maxs)\n"
    pycomm+="\n"
    pycomm+="def plotData(fileNames, iFile, doHold, axes, Color, scales, units, maxs):\n"
    pycomm+="\n"
    pycomm+="  naxes=np.shape(axes)[0]\n"
    pycomm+="\n"
    pycomm+="  makeSubPlot(fileNames[iFile], 2, axes[0][iFile], doHold, Color, '$param1', '$param2', 'Ops/sec', scales[0], units[0], maxs[0]);\n"
    pycomm+="\n"
    pycomm+="  if naxes > 1 and axes[1] != None:\n"
    pycomm+="    makeSubPlot(fileNames[iFile], 3, axes[1][iFile], doHold, Color, '$param1', '$param2', 'Writes/sec', scales[1], units[1], maxs[1]);\n"
    pycomm+="\n"
    pycomm+="  if naxes > 2 and axes[2] != None:\n"
    pycomm+="    makeSubPlot(fileNames[iFile], 4, axes[2][iFile], doHold, Color, '$param1', '$param2', 'Bytes/sec', scales[2], units[2], maxs[2]);\n"
    pycomm+="\n"
    pycomm+="def plotDataTwo(fileNames, fileNames2, action, iFile, doHold, axes, Color, scales, units, maxs):\n"
    pycomm+="\n"
    pycomm+="  naxes=np.shape(axes)[0]\n"
    pycomm+="\n"
    pycomm+="  makeSubPlotTwo(fileNames[iFile], fileNames2[iFile], 2, action, axes[0][iFile], doHold, Color, '$param1', '$param2', 'Ops/sec', scales[0], units[0], maxs[0]);\n"
    pycomm+="\n"
    pycomm+="  if naxes > 1 and axes[1] != None:\n"
    pycomm+="    makeSubPlotTwo(fileNames[iFile], fileNames2[iFile], 3, action, axes[1][iFile], doHold, Color, '$param1', '$param2', 'Writes/sec', scales[1], units[1], maxs[1]);\n"
    pycomm+="\n"
    pycomm+="  if naxes > 2 and axes[2] != None:\n"
    pycomm+="    makeSubPlotTwo(fileNames[iFile], fileNames2[iFile], 4, action, axes[2][iFile], doHold, Color, '$param1', '$param2', 'Bytes/sec', scales[2], units[2], maxs[2]);\n"
    pycomm+="\n"

    pycomm+="def getDataAndUnits(dat, nline, index):\n"
    pycomm+="  d=dat[0:nline,index];\n"
    pycomm+="  max=np.max(d);\n"
    pycomm+="  unit='';\n"
    pycomm+="  \n"
    pycomm+="  if False:\n"
    pycomm+="    if max > 1e6:\n"
    pycomm+="      d = d/1e6;\n"
    pycomm+="      unit = 'M';\n"
    pycomm+="    elif max > 1e3:\n"
    pycomm+="      d = d/1e3;\n"
    pycomm+="      unit = 'K';\n"
    pycomm+="  \n"
    pycomm+="  return [d, unit];\n"
    pycomm+="\n"

    if [ $figsize == \"\" ]; then
	pycomm+="fig = plt.figure();\n"
    else
	pycomm+="fig = plt.figure(figsize=${figsize//\"/});\n"
    fi

    pycomm+="fig.set_facecolor('white');\n"
    pycomm+="\n"

    #------------------------------------------------------------
    # Generate a list of data files
    #------------------------------------------------------------
    
    iIter="0"
    fileNames=""
    first=true
    for i in $files; do
	if [ $first == "true" ]; then
	    fileNames+="['/tmp/dat"$iIter".txt'"
	    first=false
	else
	    fileNames+=", '/tmp/dat"$iIter".txt'"
	fi
        iIter=$[$iIter+1]
    done
    fileNames+="]"

    #------------------------------------------------------------
    # If plotting with other files, generate a unified list of all
    # files to scale to
    #------------------------------------------------------------
    
    if [ $plotwith == "true" ]; then
	nFile=$iIter
	plotFileNames=""
	first=true
	for i in $files; do
	    if [ $first == "true" ]; then
		plotFileNames+="['/tmp/dat"$iIter".txt'"
		first=false
	    else
		plotFileNames+=", '/tmp/dat"$iIter".txt'"
	    fi
            iIter=$[$iIter+1]
	done
	plotFileNames+="]"
    fi

    #------------------------------------------------------------
    # If scaling to a different set of files, generate a list of those
    # files now
    #------------------------------------------------------------
    
    if [ $scale == "true" ]; then
	nFile=$iIter
	scaleFileNames=""
	first=true
	for i in $files; do
	    if [ $first == "true" ]; then
		scaleFileNames+="['/tmp/dat"$iIter".txt'"
		first=false
	    else
		scaleFileNames+=", '/tmp/dat"$iIter".txt'"
	    fi
            iIter=$[$iIter+1]
	done
	scaleFileNames+="]"
    fi

    #------------------------------------------------------------
    # Generate a list of cell sizes
    #------------------------------------------------------------
    
    iIter="0"
    cells=""
    first=true
    echo "Cellsizes = $cellsizes"
    for i in $cellsizes; do
	if [ $first == "true" ]; then
	    cells+="[$i"
	    first=false
	else
	    cells+=", $i"
	fi
        iIter=$[$iIter+1]
    done
    cells+="]"

    #------------------------------------------------------------
    # Write the file names
    #------------------------------------------------------------
    
    pycomm+="files=$fileNames;\n"
    pycomm+="colors=['b', 'c', 'm', 'g', 'y', 'k'];\n"
    pycomm+="cellsizes=$cells;\n"
    pycomm+="\n"

    #------------------------------------------------------------
    # Write any files we are overplotting
    #------------------------------------------------------------
    
    if [ $plotwith == "true" ]; then
	pycomm+="plotwithfiles=$plotFileNames;\n"
    fi

    #------------------------------------------------------------
    # If scaling to another set of files, write those and scale to
    # them, else scale to the combination of the primary and secondary
    # plot files, else just scale to the files
    #------------------------------------------------------------
    
    if [ $scale == "true" ]; then
	pycomm+="scalefiles=$scaleFileNames;\n"
	pycomm+="scales,units,maxs  = getScalesAndUnits(scalefiles);\n"
    elif [ $plotwith == "true" ]; then
	pycomm+="scales,units,maxs  = getScalesAndUnits(files + plotwithfiles);\n"
    else
	pycomm+="scales,units,maxs  = getScalesAndUnits(files);\n"
    fi


    pycomm+="plt.axis('off')\n"
    title=${title//"\n"/"\\\\n"}
    pycomm+="plt.title('${title//\"/}')\n"
    
    if [ $overplot == \""true\"" ]; then
	pycomm+="axes = getSubplots(files, True)\n"
    else
        pycomm+="axes = getSubplots(files, False)\n"
    fi

    pycomm+="\n"

    if [ $plotwith == "true" ]; then
	pycomm+="plotFiles(files, plotwithfiles, '${plotwithaction//\"/}', axes, colors, scales, units, maxs)\n"
    else
	pycomm+="plotFiles(files, None, None, axes, colors, scales, units, maxs)\n"
    fi

#    pycomm+="plt.tight_layout(w_pad=2,pad=5)\n"
#    pycomm+="print str(plt.rcParams)\n"
	
    if [ "$labels" != \"\" ]; then

	pycomm+="nFile=np.shape(files)[0]\n"
	pycomm+="top=plt.rcParams['figure.subplot.top']\n"
	pycomm+="bottom=plt.rcParams['figure.subplot.bottom']\n"
	pycomm+="hspace=plt.rcParams['figure.subplot.hspace']\n"
	pycomm+="yrange=top-bottom\n"
	pycomm+="yint = (yrange - hspace) / nFile\n"
	pycomm+="if nFile > 1:"
	pycomm+="  sint = hspace / (nFile-1)\n"
	pycomm+="else:"
	pycomm+="  sint = hspace / (nFile)\n"
	pycomm+="\n"

	IFS=';' read -ra labarr <<< "$labels"
	labarrsize=${#labarr[@]}

	echo "Labarr size = $labarrsize"

	if [ $overplot == \""false\"" ] && [ $labarrsize -gt 1 ]; then
	    label=${labarr[$iter]//\"/}

	    iIter="0"
	    pyLabs=""
	    for label in "${labarr[@]}"; do

		echo "Label = $label"

		label=${label//"\n"/"\\\\n"}
		
		if [ $iIter -eq 0 ]	; then
		    pyLabs+="['"${label//\"/}"'"
		else
		    pyLabs+=", '"${label//\"/}"'"
		fi
		iIter=$[$iIter+1]
	    done

	    pyLabs+="]"
	    pycomm+="labels = $pyLabs\n"
	    pycomm+="for i in range(0,nFile):\n"
	    pycomm+="  y = top - (i + 0.5)*yint - i*sint\n"
	    pycomm+="  plt.figtext(0.03, y, labels[i], fontsize=defaultFontsize)\n"
	else
	    labels=${labels//"\n"/"\\\\n"}
	    pycomm+="plt.figtext(0.03, 0.5, '${labels//\"/}', fontsize=defaultFontsize)\n"
	fi
    fi

    if [ $output == \"\" ]; then
	pycomm+="plt.show();\n"
    else
	pycomm+="plt.savefig('${output//\"/}.png', format='png', dpi=fig.dpi);\n"
    fi

    printf "$pycomm" > /tmp/pyplottest.py
    printf "$pycomm" | python
}

#-----------------------------------------------------------------------
# Plot data from a framework logfile:
#
# Use like:
#          plotlogfile filename param1 param2 [cellsize=arr]
#
#-----------------------------------------------------------------------

plotlogfile()
{
    cellsize=$(valOrDef cellsize '' "$@")
    overplot=$(valOrDef overplot false "$@")
    figsize=$(valOrDef figsize '' "$@")
    labels=$(valOrDef labels '' "$@")
    title=$(valOrDef title '' "$@")
    scaleto=$(valOrDef scaleto '' "$@")
    plotwithfiles=$(valOrDef plotwith '' "$@")
    plotwithaction=$(valOrDef plotwithaction 'p' "$@")
    stat=$(valOrDef stat 'ops' "$@")

    
    output=$(valOrDef output '' "$@")
    figview=$(valOrDef figview '' "$@")

    files="$1"

    allfiles=$files
    allcellsize=$cellsize
    scale=false
    plotwith=false
    
    #------------------------------------------------------------
    # If overplotting with another set of files, add those files to
    # the allfiles list for extraction, and duplicate the cellsize
    # parameters for those files
    #------------------------------------------------------------

    if [ "$plotwithfiles" != \"\" ]; then
	plotwith=true
	allfiles=$allfiles" "${plotwithfiles//\"/}
	allcellsize=$allcellsize" "$cellsize
    fi

    #------------------------------------------------------------
    # If scaling to another set of files, add those files to the
    # allfiles list for extraction, and duplicate the cellsize
    # parameters for those files
    #------------------------------------------------------------
    
    if [ "$scaleto" != \"\" ]; then
	scale=true
	allfiles=$allfiles" "${scaleto//\"/}
	allcellsize=$allcellsize" "$cellsize
    fi

    getTestData "$allfiles" $2 $3 "$allcellsize" $stat
    echo "output(1) = $output"
    if [ $output == \"\" ]; then
	echo "output is null"
    fi

    echo "allcells = $allcellsize"
    
    generatePythonPlots "$1" $param1 $param2 $overplot $figsize "$labels" "$title" $scale $plotwith $output "$figview" $plotwithaction "$allcellsize"
}

plotlogfileycsb()
{
    local files=$(valOrDef files '' "$@")
    files=${files//\"/}

    local param1=$(valOrDef param1 '' "$@")
    param1=${param1//\"/}

    local param2=$(valOrDef param2 '' "$@")
    param2=${param2//\"/}

    cellsize=$(valOrDef cellsize '' "$@")

    batchsize=$(valOrDef batchsize '' "$@")
    batchsize=${batchsize//\"/}
    
    fieldcount=$(valOrDef fieldcount '' "$@")
    overplot=$(valOrDef overplot false "$@")
    figsize=$(valOrDef figsize '' "$@")
    labels=$(valOrDef labels '' "$@")
    title=$(valOrDef title '' "$@")
    scaleto=$(valOrDef scaleto '' "$@")
    plotwithfiles=$(valOrDef plotwith '' "$@")
    plotwithaction=$(valOrDef plotwithaction 'p' "$@")

    stat=$(valOrDef stat 'ops' "$@")
    stat=${stat//\"/}
    
    output=$(valOrDef output '' "$@")
    figview=$(valOrDef figview '' "$@")

    contour=$(valOrDef contour '' "$@")
    contour=${contour//\"/}

    echo "figsize = $figsize"
    echo "labels  = '$labels'"
    
    allfiles=$files
    allcellsize=$cellsize
    allbatchsize=$batchsize
    allfieldcount=$fieldcount
    scale=false
    plotwith=false
    
    #------------------------------------------------------------
    # If overplotting with another set of files, add those files to
    # the allfiles list for extraction, and duplicate the cellsize
    # parameters for those files
    #------------------------------------------------------------

    if [ "$plotwithfiles" != \"\" ]; then
	plotwith=true
	allfiles=$allfiles" "${plotwithfiles//\"/}
	allcellsize=$allcellsize" "$cellsize
	allbatchsize=$allbatchsize" "$batchsize
	allfieldcount=$allfieldcount" "$fieldcount
    fi

    #------------------------------------------------------------
    # If scaling to another set of files, add those files to the
    # allfiles list for extraction, and duplicate the cellsize
    # parameters for those files
    #------------------------------------------------------------
    
    if [ "$scaleto" != \"\" ]; then
	scale=true
	allfiles=$allfiles" "${scaleto//\"/}
	allcellsize=$allcellsize" "$cellsize
	allbatchsize=$allbatchsize" "$batchsize
	allfieldcount=$allfieldcount" "$fieldcount
    fi

    getTestDataYcsb files="$allfiles" param1=$param1 param2=$param2 cellsize="$allcellsize" batchsize="$allbatchsize" fieldcount="$allfieldcount" stat=$stat

    echo "output(1) = $output"
    if [ $output == \"\" ]; then
	echo "output is null"
    fi

    echo "allcells = $allcellsize figsize=$figsize"
    
    generatePythonPlots "$1" $param1 $param2 $overplot $figsize "$labels" "$title" $scale $plotwith $output "$figview" $plotwithaction "$allcellsize" contour=$contour
}

makeycsbplot()
{
    output=$(valOrDef output '' "$@")
    output=${output//\"/}

    figsize=$(valOrDef figsize '(15,6)' "$@")
    figsize=${figsize//\"/}

#    plotlogfileycsb "output.log output.log" threadcount fieldcount cellsize="1 1" figsize="$figsize" labels="Cellsize=1 Cellsize=1" title="Riak PUT (YCSB)"

    #    plotlogfileycsb "ycsb2.log ycsb2.log" threadcount fieldcount cellsize="1 1" figsize="$figsize" labels="Cellsize=1 Cellsize=1" title="Riak PUT (YCSB)"

    plotlogfileycsb $output threadcount fieldcount cellsize="10" figsize="$figsize" labels="Cellsize=10" title="Riak PUT (YCSB)"
}

make5by5plots()
{
    plotlogfileycsb files="ycsb_5by5_10.out ycsb_5by5_100.out" param1=threadcount param2=fieldcount figsize="(18,12)" labels="5by5, 10Bytes;5by5, 100Bytes" title="Riak PUT Throughput (YCSB)" batchsize=1
}

make10byteplots()
{
    plotlogfileycsb files="ycsb_5by5_10.out ycsb_10by10_10.out ycsb_15by15_10.out" param1=threadcount param2=fieldcount figsize="(18,18)" labels="5 + 5 Cluster\n10-byte cols;10 + 10 Cluster\n10-byte cols;15 + 15 Cluster\n10-byte cols" title="RiakTS PUT Throughput (YCSB)" batchsize=1 contour=none output=$RIAK_TEST_BASE/images/nodeComp10Byte
}

make10colbatchplots()
{
    plotlogfileycsb files="ycsb_5by5_100byte_10col_batch.out ycsb_10by10_100byte_10col_batch.out ycsb_15by15_100byte_10col_batch.out " param1=threadcount param2=batchsize figsize="(18,18)" labels="5+5 Cluster\n100 Bytes per column\n10 Cols per row\nBatched;10+10 Cluster\n100 Bytes per column\n10 Cols per row\nBatched;15+15 Cluster\n100 Bytes per column\n10 Cols per row\nBatched" title="RiakTS PUT Throughput (YCSB)" contour=none
}

make100colbatchplots()
{
    plotlogfileycsb files="ycsb_5by5_100byte_100col_batch.out ycsb_10by10_100byte_100col_batch.out ycsb_15by15_100byte_100col_batch.out " param1=threadcount param2=batchsize figsize="(18,18)" labels="5 + 5 Cluster\n100-byte cols\n100-col rows\nBatched;10 + 10 Cluster\n100-byte cols\n100-col rows\nBatched;15 + 15 Cluster\n100-byte cols\n100-col rows\nBatched" title="RiakTS PUT Throughput (YCSB)" contour=none output=$RIAK_TEST_BASE/images/nodeComp100Byte100ColBatch
}

make100byteplots()
{
    plotlogfileycsb files="ycsb_5by5_100.out ycsb_10by10_100.out ycsb_15by15_100.out" param1=threadcount param2=fieldcount figsize="(18,18)" labels="5 + 5 Cluster\n100-byte cols;10 + 10 Cluster\n100-byte cols;15 + 15 Cluster\n100-byte cols" title="RiakTS PUT Throughput (YCSB)" batchsize=1 contour=none
    #output=$RIAK_TEST_BASE/images/nodeComp100Byte
}

get10bytedata()
{
    getTestDataYcsb files="ycsb_5by5_10.out ycsb_7by5_10.out ycsb_10by10_10.out ycsb_15by15_10.out" param1=threadcount param2=fieldcount batchsize=1
    mv /tmp/dat0.txt $RIAK_TEST_BASE/data/ycsb_5by5_10.txt
    mv /tmp/dat1.txt $RIAK_TEST_BASE/data/ycsb_7by5_10.txt
    mv /tmp/dat2.txt $RIAK_TEST_BASE/data/ycsb_10by10_10.txt
    mv /tmp/dat3.txt $RIAK_TEST_BASE/data/ycsb_15by15_10.txt
}

get100bytedata()
{
    getTestDataYcsb files="ycsb_5by5_100.out ycsb_10by10_100.out ycsb_15by10_100.out ycsb_15by15_100.out" param1=threadcount param2=fieldcount batchsize=1
    mv /tmp/dat0.txt $RIAK_TEST_BASE/data/ycsb_5by5_100.txt
    mv /tmp/dat1.txt $RIAK_TEST_BASE/data/ycsb_10by10_100.txt
    mv /tmp/dat2.txt $RIAK_TEST_BASE/data/ycsb_15by10_100.txt
    mv /tmp/dat3.txt $RIAK_TEST_BASE/data/ycsb_15by15_100.txt
}

get100bytebatchdata()
{
    getTestDataYcsb files="ycsb_5by5_100byte_100col_batch.out ycsb_10by10_100byte_100col_batch.out ycsb_15by15_100byte_100col_batch.out" param1=threadcount param2=batchsize
    mv /tmp/dat0.txt ycsb_5by5_100byte_100col.txt
    mv /tmp/dat1.txt ycsb_10by10_100byte_100col.txt
    mv /tmp/dat2.txt ycsb_15by15_100byte_100col.txt
}

make100byteratioplots()
{
    plotlogfileycsb files="ycsb_10by10_100.out ycsb_15by15_100.out" param1=threadcount param2=fieldcount figsize="(18,12)" labels="10+10 Cluster\n100 Bytes;15+15 Cluster\n100 Bytes" title="RiakTS PUT Throughput (YCSB)\n" batchsize="1 1" plotwithaction='/' plotwith="ycsb_5by5_100.out ycsb_5by5_100.out" contour=contouronly
}

make15by15plots()
{
    plotlogfileycsb files="ycsb_15by15_10.out ycsb_15by15_100.out" param1=threadcount param2=fieldcount figsize="(18,6)" labels="15by15, 10Bytes; 100 Bytes" title="RiakTS PUT Throughput (YCSB)" batchsize=1
}

makeexcessharnessplots()
{
    plotlogfileycsb files="ycsb_5by5_10.out ycsb_7by5_10.out" param1=threadcount param2=fieldcount figsize="(18,6)" labels="5by5\n10 Bytes;10by10\n100 Bytes" title="RiakTS PUT Throughput (YCSB)" batchsize="1 1" overplot=true
}

make10by10plots()
{
    plotlogfileycsb files="ycsb_10by10_10.out ycsb_10by10_100.out" param1=threadcount param2=fieldcount figsize="(18,6)" labels="10by10, 10Bytes;10by10, 100Bytes" title="RiakTS PUT Throughput (YCSB)" batchsize=1
}

make5by5batchplot()
{
    plotlogfileycsb files="ycsb_5by5_100byte_10col_batch.out ycsb_5by5_100byte_100col_batch.out" param1=threadcount param2=batchsize figsize="(18,6)" labels="5by5 100Bytes\n10Cols batched;5by5 100Bytes\n100Cols batched" title="RiakTS PUT Throughput (YCSB)"
}

make10by10batchplot()
{
    plotlogfileycsb files="ycsb_10by10_100byte_10col_batch.out ycsb_10by10_100byte_100col_batch.out" param1=threadcount param2=batchsize figsize="(18,6)" labels="10by10 100Bytes\n10Cols batched;10by10 100Bytes\n100Cols batched" title="RiakTS PUT Throughput (YCSB)"
}

make15by15batchplot()
{
    plotlogfileycsb files="ycsb_15by15_100byte_10col_batch.out ycsb_15by15_100byte_100col_batch.out" param1=threadcount param2=batchsize figsize="(18,6)" labels="15by15 100Bytes\n10Cols batched;15by15 100Bytes\n100Cols batched" title="RiakTS PUT Throughput (YCSB)"
}

makeplot()
{
    output=$(valOrDef output '' "$@")
    output=${output//\"/}

    figsize=$(valOrDef figsize '(15,18)' "$@")
    figsize=${figsize//\"/}
    
    plotlogfile "riak_sjb_thread_v_columns_1.log riak_sjb_thread_v_columns_10.log riak_sjb_thread_v_columns_100.log riak_sjb_thread_v_columns_200.log" threads columns cellsize="1 10 100 200" figsize="$figsize" labels="Cellsize=1 Cellsize=10 Cellsize=100 Cellsize=200" output=$output title="Riak PUT (SJB)"
    
#    plotlogfile "riak_sjb_thread_v_columns_1.log riak_sjb_thread_v_columns_10.log riak_sjb_thread_v_columns_100.log" threads columns cellsize="1 10 100" figsize="(18,5)" overplot=false

#    plotlogfile "riak_sjb_thread_v_columns_100.log" threads columns cellsize="100" figsize="(18,5)" overplot=false
}

makeplots()
{
    plotlogfile "threads_v_columns_riak_v3.log threads_v_columns_riak_v2.log threads_v_columns_riak_v6.log threads_v_columns_riak_v5.log threads_v_columns_riak_v4.log" threads columns "1 10 200 500 1000"
}

makeRiakPlots()
{
    output=$(valOrDef output '' "$@")
    output=${output//\"/}

    plotlogfile "threads_v_columns_riak_v3.log threads_v_columns_riak_v2.log threads_v_columns_riak_v7.log threads_v_columns_riak_v6.log threads_v_columns_riak_v5.log" threads columns cellsize="1 10 100 200 500" overplot=false figsize="(16,18)" labels="Cellsize=1 Cellsize=10 Cellsize=100 Cellsize=200 Cellsize=500" title="Riak SJB threads vs. columns" output=$output
}

makeRiakNvalPlots()
{
    output=$(valOrDef output '' "$@")
    output=${output//\"/}

    action=$(valOrDef action 'p' "$@")
    action=${action//\"/}

    plotlogfile "threads_v_columns_riak_v3.log threads_v_columns_riak_v2.log threads_v_columns_riak_v7.log threads_v_columns_riak_v6.log threads_v_columns_riak_v5.log" threads columns cellsize="1 10 100 200 500" overplot=false figsize="(16,18)" labels="Cellsize=1 Cellsize=10 Cellsize=100 Cellsize=200 Cellsize=500" title="Riak SJB threads vs. columns, nval=1 vs nval=3\\\\n(cyan: nval=1, magenta: nval=3)" plotwith="riak_sjb_thread_v_columns_1_nval3.log riak_sjb_thread_v_columns_10_nval3.log riak_sjb_thread_v_columns_100_nval3.log riak_sjb_thread_v_columns_200_nval3.log riak_sjb_thread_v_columns_500_nval3.log" figview="(30,135)" output=$output plotwithaction=$action
}

makeRiakCassOverplots()
{
    output=$(valOrDef output '' "$@")
    output=${output//\"/}
    
    plotlogfile "threads_v_columns_riak_v3.log threads_v_columns_riak_v2.log threads_v_columns_riak_v7.log threads_v_columns_riak_v6.log threads_v_columns_riak_v5.log" threads columns cellsize="1 10 100 200 500" overplot=false figsize="(16,18)" labels="Cellsize=1 Cellsize=10 Cellsize=100 Cellsize=200 Cellsize=500" title="Riak + Cass SJB threads vs. columns\\\\n(cyan = Riak, magenta = Cass)" plotwith="cass_sjb_thread_v_columns_1.log cass_sjb_thread_v_columns_10.log cass_sjb_thread_v_columns_100.log cass_sjb_thread_v_columns_200.log cass_sjb_thread_v_columns_500.log" output=$output
}

makeCassPlots()
{
    output=$(valOrDef output '' "$@")
    output=${output//\"/}

    plotlogfile "cass_sjb_thread_v_columns_1.log cass_sjb_thread_v_columns_10.log cass_sjb_thread_v_columns_100.log cass_sjb_thread_v_columns_200.log cass_sjb_thread_v_columns_500.log" threads columns cellsize="1 10 100 200 500" overplot=false figsize="(16,18)" labels="Cellsize=1 Cellsize=10 Cellsize=100 Cellsize=200 Cellsize=500" title="Cass SJB threads vs. columns" scaleto="threads_v_columns_riak_v3.log threads_v_columns_riak_v2.log threads_v_columns_riak_v7.log threads_v_columns_riak_v6.log threads_v_columns_riak_v5.log" output=$output
}

runAllCassSjb()
{
    batchRunCassSjb cellsize="10 200 500"
}

runAllRiakSjb()
{
    batchRunRiakSjb cellsize="1 10 100 200 500"
}

batchRunCassSjb()
{
    cellsize=$(valOrDef cellsize '' "$@")
    cellsize=${cellsize//\"/}

    for cell in $cellsize
    do
	runCassSjb cellsize=$cell
    done
}

batchRunRiakSjb()
{
    cellsize=$(valOrDef cellsize '' "$@")
    cellsize=${cellsize//\"/}

    for cell in $cellsize
    do
	runRiakSjb cellsize=$cell
    done
}

runRiakSjb()
{
    cellsize=$(valOrDef cellsize '' "$@")
    cellsize=${cellsize//\"/}

    suffix=$(valOrDef suffix '' "$@")
    suffix=${suffix//\"/}
    
    cd=`pwd`
    echo "test = \"$cd/lib/tests/riak-simple_java_bench\";" > riak.run
    parentdir="$(dirname "$(pwd)")"
    echo "hosts = \"$parentdir/etc/hosts.d/softlayer-b\";" >> riak.run
    echo "" >> riak.run

    echo "simple_java_bench = {" >> riak.run
    echo "         threads = [32, 64, 128, 256];" >> riak.run
    #    echo "         columns = [1, 5, 10, 15];" >> riak.run
    echo "         columns = [1, 10, 200, 500];" >> riak.run
    echo "         cell_size = $cellsize;" >> riak.run
    echo "}" >> riak.run
    echo "" >> riak.run
    echo "riak_ts = {" >> riak.run
    echo "         object.size.maximum = 100GB;" >> riak.run
    echo "         object.size.warning_threshold = 100GB;" >> riak.run
    echo "}" >> riak.run

    source $parentdir/etc/profile.d/internal_utilities.sh 
    source activate

    hosts softlayer-b

    if [ -z $suffix ]; then
	outputFile="riak_sjb_thread_v_columns_"$cellsize".log"
    else
	outputFile="riak_sjb_thread_v_columns_"$cellsize"_"$suffix".log"
    fi
    
    basho-perf run riak.run &> $outputFile
}

runRiakSjbGeneric()
{
    cellsize=$(valOrDef cellsize '1' "$@")
    cellsize=${cellsize//\"/}

    columns=$(valOrDef columns '[1,5,10,15]' "$@")
    columns=${columns//\"/}

    threads=$(valOrDef threads '[32,64,128,256]' "$@")
    threads=${threads//\"/}

    suffix=$(valOrDef suffix '' "$@")
    suffix=${suffix//\"/}

    branch=$(valOrDef branch '' "$@")
    branch=${branch//\"/}

    bashoPerfDir="$(dirname "${RIAK_TEST_BASE}")"
    internalUtilDir="$(dirname "$bashoPerfDir")"

    echo "test = \"$bashoPerfDir/lib/tests/riak-simple_java_bench\";" > riak.run
    echo "hosts = \"$internalUtilDir/etc/hosts.d/softlayer-b\";" >> riak.run
    echo "" >> riak.run

    echo "simple_java_bench = {" >> riak.run
    echo "         threads = $threads;" >> riak.run
    echo "         columns = $columns;" >> riak.run
    echo "         cell_size = $cellsize;" >> riak.run
    echo "}" >> riak.run

    echo "branch = $branch"
    
    if [ "$branch" != "" ]
    then
	echo "riak_ts = {" >> riak.run
	echo "         branch = $branch;" >> riak.run
	echo "}" >> riak.run
    fi
    
    source $internalUtilDir/etc/profile.d/internal_utilities.sh 
    source activate

    hosts softlayer-b

    if [ -z $suffix ]; then
	outputFile="riak_sjb_thread_v_columns_"$cellsize".log"
    else
	outputFile="riak_sjb_thread_v_columns_"$cellsize"_"$suffix".log"
    fi
    
    basho-perf run riak.run &> $outputFile
}

testRunCassSjb()
{
    cellsize=$(valOrDef cellsize '' "$@")
    cellsize=${cellsize//\"/}
    echo "cellsize = $cellsize"
}

runCassSjb()
{
    cellsize=$(valOrDef cellsize '' "$@")
    cellsize=${cellsize//\"/}
    
    cd=`pwd`
    echo "test = \"$cd/lib/tests/cassandra-simple_java_bench\";" > cass.run
    parentdir="$(dirname "$(pwd)")"
    echo "hosts = \"$parentdir/etc/hosts.d/softlayer-b\";" >> cass.run
    echo "" >> cass.run

    echo "simple_java_bench = {" >> cass.run
    echo "         threads = [8, 16, 32, 64, 128, 256];" >> cass.run
    echo "         columns = [1, 5, 10, 15];" >> cass.run
    echo "         cell_size = $cellsize;" >> cass.run
    echo "}" >> cass.run

    source $parentdir/etc/profile.d/internal_utilities.sh 
    source activate

    hosts softlayer-b
    
    outputFile="cass_sjb_thread_v_columns_"$cellsize".log"
    basho-perf run cass.run &> $outputFile
}

localTimeToUtc()
{
    local timeanddate="$1"

    local pycomm="import pytz, datetime\n"
    pycomm+="local = pytz.timezone (\"America/Los_Angeles\")\n"
    pycomm+="naive = datetime.datetime.strptime (\"$timeanddate\", \"%%Y-%%m-%%d %%H:%%M:%%S\")\n"
    pycomm+="local_dt = local.localize(naive, is_dst=None)\n"
    pycomm+="utc_dt = local_dt.astimezone(pytz.utc)\n"
    pycomm+="print utc_dt.strftime(\"%%Y-%%m-%%dT%%H:%%M:%%SZ\")\n"

    printf "$pycomm" >> /tmp/timeTest.py

    printf "$pycomm" | python
}

constructInfluxQuery()
{
    starttime="$1"
    stoptime="$2"

    utcstart=$(localTimeToUtc "$starttime")
    utcstop=$(localTimeToUtc "$stoptime")
    
    query="select min(value), max(value) from cpu_value where time > '$utcstart' and time < '$utcstop' and ( host = 'basho-c4s1' or host = 'basho-c4s2' or host = 'basho-c4s3' or host = 'basho-c4s4' or host = 'basho-c4s5' ) group by type_instance"

    echo $query >> /tmp/constructQueries.txt
    
    wget -qO - "http://localhost:58086/query?db=collectd&q=$query"
}

pythonInfluxQuery()
{
    starttime="$1"
    stoptime="$2"

    utcstart=$(localTimeToUtc "$starttime")
    utcstop=$(localTimeToUtc "$stoptime")
    
    query="http://localhost:58086/query?db=collectd&q=select min(value), max(value) from cpu_value where time > '$utcstart' and time < '$utcstop' and ( host = 'basho-c4s1' or host = 'basho-c4s2' or host = 'basho-c4s3' or host = 'basho-c4s4' or host = 'basho-c4s5' ) group by type_instance"

    echo $query >> /tmp/queries.txt
    
    local pycomm="import requests\n"
    pycomm+="\n"
    pycomm+="r = requests.get(\"$query\")\n"

    pycomm+="total = 0.0\n"
    pycomm+="cpu_ticks = {}\n"
    pycomm+="for v in r.json()['results'][0]['series']:\n"
    pycomm+="  instance = v['tags']['type_instance']\n"
    pycomm+="  ticks = int(v['values'][0][2]) - int(v['values'][0][1])\n"
    pycomm+="  cpu_ticks[instance] = ticks\n"
    pycomm+="  total += ticks\n"
    pycomm+="\n"
    pycomm+="cpu_usage = 0.0\n"
    pycomm+="for key in cpu_ticks.keys():\n"
    pycomm+="  if key != 'idle':\n"
    pycomm+="    cpu_usage += cpu_ticks[key]\n"
    pycomm+="\n"
    pycomm+="print round((1.0 - cpu_usage/total) * 100.0, 2)\n"
    pycomm+="\n"

    printf "$pycomm" | python
    printf "$pycomm" >> /tmp/queryTest.py
}

pythonInfluxPrint()
{
    starttime="$1"
    stoptime="$2"

    utcstart=$(localTimeToUtc "$starttime")
    utcstop=$(localTimeToUtc "$stoptime")
    
    query="http://localhost:58086/query?db=collectd&q=select min(value), max(value) from cpu_value where time > '$utcstart' and time < '$utcstop' and ( host = 'basho-c4s1' or host = 'basho-c4s2' or host = 'basho-c4s3' or host = 'basho-c4s4' or host = 'basho-c4s5' ) group by type_instance"

    echo $query >> /tmp/queries.txt
    
    local pycomm="import requests\n"
    pycomm+="\n"
    pycomm+="r = requests.get(\"$query\")\n"

    pycomm+="total = 0.0\n"
    pycomm+="cpu_ticks = {}\n"
    pycomm+="for v in r.json()['results'][0]['series']:\n"
    pycomm+="  instance = v['tags']['type_instance']\n"
    pycomm+="  ticks = int(v['values'][0][2]) - int(v['values'][0][1])\n"
    pycomm+="  cpu_ticks[instance] = ticks\n"
    pycomm+="  total += ticks\n"
    pycomm+="\n"
    pycomm+="print cpu_ticks\n"
    pycomm+="\n"

    printf "$pycomm" | python
}

runInfluxQuery()
{
    query="$1"

    echo $query >> /tmp/runQueries.txt
    
    wget -qO - "http://localhost:58086/query?db=collectd&q=$query"
}

showInfluxQuery()
{
    starttime="$1"
    stoptime="$2"

    utcstart=$(localTimeToUtc "$starttime")
    utcstop=$(localTimeToUtc "$stoptime")
    
    query="select min(value), max(value) from cpu_value where time > '$utcstart' and time < '$utcstop' and ( host = 'basho-c4s1' or host = 'basho-c4s2' or host = 'basho-c4s3' or host = 'basho-c4s4' or host = 'basho-c4s5' ) group by type_instance"

    echo $query
}

emltest()
{
    output=$(valOrDef output '' "$@")

    if [ $output == \"\" ]; then
	echo "hello"
    fi
}

runTsBranchComp()
{
    branch1=$(valOrDef branch1 'riak_ts_ee-1.3.1' "$@")
    branch1=${branch1//\"/}

    branch2=$(valOrDef branch2 'riak_ts_ee-1.4.0rc1' "$@")
    branch2=${branch2//\"/}

    threads=$(valOrDef threads '64' "$@")
    threads=${threads//\"/}
    
    columns=$(valOrDef columns '1' "$@")
    columns=${columns//\"/}
    
    cellsize=$(valOrDef cellsize '1' "$@")
    cellsize=${cellsize//\"/}
    
    type=$(valOrDef type 'put' "$@")

    runRiakSjbGeneric threads=$threads columns=$columns cellsize=$cellsize suffix="branch1" branch=$branch1
    runRiakSjbGeneric threads=$threads columns=$columns cellsize=$cellsize suffix="branch2" branch=$branch2
}

#-----------------------------------------------------------------------
# Script to compare two branches of TS (currently, using SJB with puts
# only, because YCSB is under development)
#-----------------------------------------------------------------------

runTsBranchComp1D()
{
    runTsBranchComp threads="64" columns="[1,1,1]" cellsize="1"
}

#-----------------------------------------------------------------------
# Generate python plots of the data we scraped from the logfile
#-----------------------------------------------------------------------

generatePythonComp()
{
    files="$1"

    local pycomm="import scipy.stats as stats;\n"
    pycomm+="import numpy as np;\n"
    pycomm+="\n"
    pycomm+="def getMeanAndStd(fileNames):\n"
    pycomm+="\n"
    pycomm+="  means = []\n"
    pycomm+="  stds = []\n"
    pycomm+="  iFile=0\n"
    pycomm+="  ncolmax=0\n"
    pycomm+="\n"
    pycomm+="  for file in fileNames:\n"
    pycomm+="    dat = np.loadtxt(file);\n"
    pycomm+="    nline = np.shape(dat)[0];\n"
    pycomm+="    vals  = dat[0:nline, 3]\n"
    pycomm+="    means.append(np.mean(vals))\n"
    pycomm+="    stds.append(np.std(vals, ddof=1))\n"
    pycomm+="\n"
    pycomm+="  return means,stds\n"
    pycomm+="\n"

    #------------------------------------------------------------
    # Generate a list of data files
    #------------------------------------------------------------
    
    iIter="0"
    fileNames=""
    first=true
    for i in $files; do
	if [ $first == "true" ]; then
	    fileNames+="['/tmp/dat"$iIter".txt'"
	    first=false
	else
	    fileNames+=", '/tmp/dat"$iIter".txt'"
	fi
        iIter=$[$iIter+1]
    done
    fileNames+="]"

    pycomm+="files=$fileNames;\n"

    pycomm+="\n"
    pycomm+="mean,std = getMeanAndStd(files)\n"
    pycomm+="print 'Means = ' + str(mean[0]) + ', ' + str(mean[1])\n"
    pycomm+="print 'Stdds = ' + str(std[0]) + ', ' + str(std[1])\n"
    
    pycomm+="chi2 = np.power((mean[0] - mean[1]),2) / (np.power(std[0],2) + np.power(std[1],2))\n"
    pycomm+="\n"
    pycomm+="print 'Chi2 = ' + str(chi2) + ' PTE = ' + str(1.0-stats.chi2.cdf(chi2, 1.0))\n"

    printf "$pycomm" > /tmp/pytest.py
    printf "$pycomm" | python
}

#------------------------------------------------------------
# Compare a single test on two branches
#------------------------------------------------------------

bashoPerfDiff1D()
{
    runTsBranchComp threads="64" columns="[1,1,1,1,1]" cellsize="1"
    
    file1=riak_sjb_thread_v_columns_1_branch1.log
    file2=riak_sjb_thread_v_columns_1_branch2.log
    
    getTestDataSingleNew $file1 threads columns 1 0 "\"ops\""
    getTestDataSingleNew $file2 threads columns 1 1 "\"ops\""

    echo ""
    
    generatePythonComp "/tmp/dat0.txt /tmp/dat1.txt"
}

bashoPerfDiff2d()
{
        echo "hello"
}

bashoPerfDiff3d()
{
        echo "hello"
}

generatePythonPlotTest()
{
    generatePythonPlots "/tmp/dat1.txt /tmp/dat2.txt" threads columns false "(16,18)" "label1 label2" title 2 12 outputtest "(30,135)" p
}

getSLRiakDir()
{
    cluster=$1
    vals=(`echo $(systemhosts $cluster) | tr " " "\n"`)
    host=${vals[0]}
    vals=`env_ssh $host 'which riak'`
    tvals=(`echo $vals | tr " " "\n"`)
    dirname `dirname ${tvals[1]}`
}

getSLBashoPerfDir()
{
    cluster=$1
    vals=(`echo $(systemhosts $cluster) | tr " " "\n"`)
    host=${vals[0]}
    vals=`env_ssh $host 'which basho-perf'`
    tvals=(`echo $vals | tr " " "\n"`)
    dirname `dirname ${tvals[1]}`
}

harnesshosts()
{
    cluster=$1
    slhosts=`hosts $cluster 2>&1`
    if [[ "$slhosts" =~ [[:print:]]*"harness_hosts: hosts (4):"([[:print:]]*) ]]
    then
	echo ${BASH_REMATCH[1]}
    fi
}

systemhosts()
{
    cluster=$1
    slhosts=`hosts $cluster 2>&1`
    if [[ "$slhosts" =~ [[:print:]]*"system_hosts:"([[:print:]]*) ]]
    then
	savestr=${BASH_REMATCH[1]}
	subs=${BASH_REMATCH[1]}
	if [[ "$subs" =~ [[:print:]]*"):"([[:print:]]*) ]]
	then
	    echo ${BASH_REMATCH[1]}
	else
	    echo $savestr
	fi
	
    fi
}

#-----------------------------------------------------------------------
# Query SL system hosts for ring size
#
# Usage:
#
#    getSLRingBytes clusterName
#
#-----------------------------------------------------------------------

getSLRingBytes()
{
    cluster=$1

    echoerr "Querying Riak dir on $cluster:\n"

    rdir=$(getSLRiakDir $cluster)

    echoerr "$(colorize $rdir "green")\n"
    echoerr "Querying basho-perf dir on $cluster:\n"
    
    bpdir=$(getSLBashoPerfDir $cluster)

    echoerr "$(colorize $bpdir "green")\n"
    echoerr "Getting system hosts for $cluster:"
    
    hosts=$(systemhosts $cluster)
    ret=""

    #------------------------------------------------------------
    # First query the ring partitions from the first system host
    #------------------------------------------------------------

    echoerr "$(colorize "$hosts" "green")\n"
    echoerr "Getting ring partitions $cluster...\n"
    
    firsthost=($hosts)
    dirlist=`env_ssh $firsthost "ls $rdir/data/leveldb"`

    ret+="ringSegments = ["
    ret+=$(commaSeparatedResponses "$dirlist")
    ret+="]\n"

    #------------------------------------------------------------
    # Now iterate over all system hosts, querying for actual ring sizes
    #------------------------------------------------------------
    
    for host in $hosts
    do
	echoerr "Getting ring size for $host\n"
	
	resp=`env_ssh $host "source $bpdir/basho-perf-scripts/prof_source > /dev/null;getRingBytes $host $rdir"`
	ret+=$(stripSLResp "$resp")
	ret+="\n"
    done
    
    printf "$ret"
}

buildSLPartitionFiles()
{
    cluster=$1

    echoerr "Querying Riak dir on $cluster:\n"
    rdir=$(getSLRiakDir $cluster)
    echoerr "$(colorize $rdir "green")\n"

    echoerr "Querying basho-perf dir on $cluster:\n"
    bpdir=$(getSLBashoPerfDir $cluster)
    echoerr "$(colorize $bpdir "green")\n"


    echoerr "Getting system hosts for $cluster:"
    hosts=$(systemhosts $cluster)
    echoerr "$(colorize "$hosts" "green")\n"
    
    #------------------------------------------------------------
    # Now iterate over all system hosts, querying for actual ring sizes
    #------------------------------------------------------------

    \rm ring.txt
    
    for host in $hosts
    do
	echoerr "Building partition file for $host\n"
	
	env_ssh $host "source $bpdir/basho-perf-scripts/prof_source > /dev/null;buildPartitionFile $rdir $host '/tmp/'$host'_ring.txt'"
	hscp $host:'/tmp/'$host'_ring.txt' .
	cat $host'_ring.txt' >> ring.txt
    done
}

buildSLPartitionFilesSF()
{
    cluster=$1

    echoerr "Querying Riak dir on $cluster:\n"
    rdir=$(getSLRiakDir $cluster)
    echoerr "$(colorize $rdir "green")\n"

    echoerr "Querying basho-perf dir on $cluster:\n"
    bpdir=$(getSLBashoPerfDir $cluster)
    echoerr "$(colorize $bpdir "green")\n"


    echoerr "Getting system hosts for $cluster:"
    hosts=$(systemhosts $cluster)
    echoerr "$(colorize "$hosts" "green")\n"
    
    #------------------------------------------------------------
    # Now iterate over all system hosts, querying for actual ring sizes
    #------------------------------------------------------------

    \rm ring.txt
    
    for host in $hosts
    do
	echoerr "Building partition file for $host\n"
	
	env_ssh $host "source $bpdir/basho-perf-scripts/prof_source > /dev/null;buildPartitionFileSF $rdir $host '/tmp/'$host'_ring.txt'"
	hscp $host:'/tmp/'$host'_ring.txt' .
	cat $host'_ring.txt' >> ring.txt
    done
}

getPartitionFiles()
{
    cluster=$1

    echoerr "Getting system hosts for $cluster:"
    hosts=$(systemhosts $cluster)
    echoerr "$(colorize "$hosts" "green")\n"
    
    #------------------------------------------------------------
    # Now iterate over all system hosts, querying for actual ring sizes
    #------------------------------------------------------------

    \rm ring.txt
    
    for host in $hosts
    do
	echoerr "Retrieving partition file for $host\n"
	
	hscp $host:'/tmp/'$host'_ring.txt' .
	cat $host'_ring.txt' >> ring.txt
    done
}

stripSLResp()
{
    resp="$1"
    ret=""

    first=true
    for item in $resp
    do
	if [ $first == true ]
	then
	    first=false
	else
	    ret+=$item
	fi
    done

    echo $ret
}

commaSeparatedResponses()
{
    resp="$1"
    ret=""

    firstitem=true
    first=true
    for item in $resp
    do
	if [ $first == true ]
	then
	    first=false
	else
	    if [ $firstitem == true ]
	    then
		firstitem=false
		ret+="'"$item"'"
	    else
		ret+=", '"$item"'"
	    fi
	    first=true
	fi
    done

    echo "$ret"
}

retrieveAnalyzerFiles()
{
    cluster=$1

    hosts=$(systemhosts $cluster)

    echo "Hosts are: $hosts"
    
    files=""
    for host in $hosts
    do
	echoerr "Getting counters for $host\n"
	hscp $host:/tmp/riak_atomicCounters.txt $host"_counters.txt"
	files+=$host"_counters.txt "
    done

    export analyzerFiles=$files
}

retrieveEventFiles()
{
    cluster=$1

    hosts=$(systemhosts $cluster)

    echo "Hosts are: $hosts"
    
    files=""
    for host in $hosts
    do
	echoerr "Getting events for $host\n"
	hscp $host:/tmp/riak_events.txt $host"_events.txt"
	files+=$host"_events.txt "
    done

    export eventFiles=$files
}

animate()
{
    # Animate 45 frames of the output files, skipping the first 175
    # frames

    skip=$(valOrDef skip '175' "$@")
    save=$(valOrDef save 'False' "$@")
    nframe=$(valOrDef nframe '45' "$@")
    tags=$(valOrDef tags 'syncput query' "$@")

    python $RIAK_TEST_BASE/python_scripts/ringanim.py files="$analyzerFiles" tags="${tags//\"/}" skipstart=${skip//\"/} nframe=${nframe//\"/} save=${save//\"/}
}

getGenBucketJson()
{
    nCol=$1
    quantum=$2
    unit=$3
    
    tableName="Gen"$nCol
    
    JSON="{\"props\": {\"n_val\": 1, \"table_def\": \"CREATE TABLE $tableName (myfamily varchar not null, myseries varchar not null, time timestamp not null, "
    
    iCol=1
    while [ $iCol -le $nCol ]
    do
	JSON+='myvar'$iCol' varchar not null, '
	iCol=$[$iCol+1]
    done

    JSON+="myint sint64 not null, "
    
    JSON+="PRIMARY KEY ((myfamily, myseries, quantum(time, "$quantum", '"$unit"')), myfamily, myseries, time))\"}}"

    echo $JSON
}

createGenBucket()
{
    nCol=$1
    quantum=$2
    unit=$3
    
    tableName="Gen"$nCol

    JSON=$(getGenBucketJson $nCol $quantum $unit)
    
    riak-admin bucket-type create $tableName ${JSON}
    sleep 3
    riak-admin bucket-type activate $tableName
}

testSequenceSL()
{
    #------------------------------------------------------------
    # Argument parsing
    #------------------------------------------------------------
    
    # How many iterations?
    
    local nIter=$(valOrDef iter '1' $@)
    nIter=${nIter//\"/}
    
    # Start iteration # ?
    
    local startIter=$(valOrDef start '0' $@)
    startIter=${startIter//\"/}

    # Location of riak?
    
    local riak=$(valOrDef riak '' "$@")
    riak=${riak//\"/}

    # Which erlang fn to run?
    
    local erlfn=$(valOrDef erlfn '' $@)
    erlfn=${erlfn//\"/}

    local args=$(valOrDef args '' "$@")
    args=${args//\"/}

    local prefix=$(valOrDef prefix '' $@)
    prefix=${prefix//\"/}
    
    endIter=$[$startIter + $nIter]

    echo $nIter

    #------------------------------------------------------------
    # Print arguments for debugging
    #------------------------------------------------------------
    
    echo "nIter     = $nIter"
    echo "startIter = $startIter"
    echo "riak      = $riak"
    echo "erlfn     = $erlfn"
    echo "args      = $args"
    echo "prefix    = $prefix"

    #------------------------------------------------------------
    # Make the output directories if they don't exist already
    #------------------------------------------------------------
    
    if [ ! -d /tmp/client_profiler_results ]; then
	mkdir /tmp/client_profiler_results
    fi

    if [ ! -d /tmp/riak_test_data ]; then
	mkdir /tmp/riak_test_data
    fi

    #------------------------------------------------------------
    # Run the script
    #------------------------------------------------------------
    
    iIter=$startIter
    while [ $iIter -lt $endIter ]
    do
	echo "Running $erlfn with args='$args'"
	runerl riak=$riak mod=riak_prof_tests fn=$erlfn args="$args"
	echo "Copying last file to " "/tmp/riak_test_data/"$prefix"_iter"$iIter".txt"
	cp `getlastSL /tmp/client_profiler_results` "/tmp/riak_test_data/"$prefix"_iter"$iIter".txt"
	iIter=$[$iIter+1]
    done
}

labeltest()
{
    labels=$(valOrDef labels '' "$@")
    labels=${labels//\"/}

    IFS=';' read -ra ADDR <<< "$labels"
    for i in "${ADDR[@]}"; do
	echo "val = '$i'"
    done
}
