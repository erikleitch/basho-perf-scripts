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

#-----------------------------------------------------------------------
# Given a throughput sum, and a set of parameters, return writes/sec, or
# empty string if none is available
#-----------------------------------------------------------------------

getWrites()
{
    local av=$1
    param1=$2
    val1=$3
    param2=$4
    val2=$5

    if [[ $param1 == 'columns' ]]; then
	echo $av"*"$val1 | bc
    elif [[ $param2 == 'columns' ]]; then
	echo $av"*"$val2 | bc
    else
	echo ""
    fi
}

getCellsize()
{
    param1=$1
    val1=$2
    param2=$3
    val2=$4
    cellsize=$5
    iter=$6

    echo "inside getCellsize with celsize=$cellsize" >> /tmp/debug.out
    
    if [ $param1 == 'cell_size' ]; then
	echo $val1
    elif [ $param2 == 'cell_size' ]; then
	echo $val2
    elif [ ! -z "$cellsize" ]; then
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
# Given a throughput sum, and a set of parameters, return bytes/sec, or
# empty string if none is available
#-----------------------------------------------------------------------


getBytes()
{
    local av=$1
    param1=$2
    val1=$3
    param2=$4
    val2=$5
    iter=$7

    echo "About to call getCellsize with 6=$6 7=$7" >> /tmp/debug.out
    
    cellsize=$(getCellsize $param1 $val1 $param2 $val2 "$6" $7)

    echo "cellsize = $cellsize" >>/tmp/debug.out
    
    if [ $param1 == 'columns' ] && [ ! -z "$cellsize" ]; then
	echo $av"*"$val1"*"$cellsize | bc
    elif [ $param2 == 'columns' ] && [ ! -z "$cellsize" ]; then
	echo $av"*"$cellsize"*"$val2 | bc
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

    total=0.0
    if [ $stat == \"ops\" ]; then
	av+=")"
	total=`echo $av | bc`

	writes=$(getWrites "$av" $param1 $val1 $param2 $val2)
	bytes=$(getBytes "$av" $param1 $val1 $param2 $val2 "$cellsize" $iter)

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
				    rm "/tmp/dat"$iter".txt"
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

    pycomm="import scipy.interpolate as int;\n"
    pycomm+="import numpy as np;\n"
    pycomm+="import matplotlib.pyplot as plt;\n"
    pycomm+="from mpl_toolkits.mplot3d import Axes3D;\n"

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
    pycomm+="      ax = fig.add_subplot(1, nColPerRow, iCol+1, projection='3d');\n"
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
    pycomm+="        axes[iCol].append(fig.add_subplot(nFile, nColPerRow, currSubplotInd, projection='3d'));\n"
    pycomm+="\n"
    pycomm+="  return axes\n"

    pycomm+="\n"
    pycomm+="def getData(fileName, index):\n"
    pycomm+="\n"
    pycomm+="  dat = np.loadtxt(fileName);\n"
    pycomm+="  nline = np.shape(dat)[0];\n"
    pycomm+="  x = dat[0:nline,0];\n"
    pycomm+="  y = dat[0:nline,1];\n"
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
    pycomm+="  z2=int.griddata(points, d, (x2, y2), method='cubic');\n"
    pycomm+="  return x2, y2, z2, unit\n"
    pycomm+="\n"
    pycomm+="def makeSubPlot(fileName, index, ax, doHold, Color, xlabel, ylabel, zlabel, scale, unit, maxVal):\n"
    pycomm+="\n"
    pycomm+="  x,y,z,unit2 = getData(fileName, index);\n"
    pycomm+="  plt.hold(doHold);\n"
    pycomm+="  ax.plot_surface(x, y, z/scale, color=Color);\n"
    pycomm+="  ax.set_xlabel('\\\\n' + xlabel);\n"
    pycomm+="  ax.set_ylabel('\\\\n' + ylabel);\n"
    pycomm+="  ax.set_zlabel('\\\\n' + zlabel + ' (' + unit + ')');\n"
    pycomm+="  ax.set_zlim(0, maxVal*1.1);\n"
    pycomm+="\n"

    pycomm+="def plotFiles(files, plotwithfiles, axes, colors, scales, units, maxs):\n"
    pycomm+="  nfile=np.shape(files)[0]\n"
    pycomm+="  ncolor=np.shape(colors)[0];\n"
    pycomm+="\n"
    pycomm+="  for iFile in range(0,nfile):\n"
    pycomm+="\n"
    pycomm+="    if plotwithfiles != None:\n"
    pycomm+="      if iFile == 0:\n"
    pycomm+="        plotData(files, iFile, False, axes, 'c', scales, units, maxs)\n"
    pycomm+="        plotData(plotwithfiles, iFile, True, axes, 'm', scales, units, maxs)\n"
    pycomm+="      else:\n"
    pycomm+="        plotData(files, iFile,  True, axes, 'c', scales, units, maxs)\n"
    pycomm+="        plotData(plotwithfiles, iFile, True, axes, 'm', scales, units, maxs)\n"
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
    pycomm+="  makeSubPlot(fileNames[iFile], 2, axes[0][iFile], doHold, Color, 'threads', 'columns', 'Ops/sec', scales[0], units[0], maxs[0]);\n"
    pycomm+="\n"
    pycomm+="  if naxes > 1 and axes[1] != None:\n"
    pycomm+="    makeSubPlot(fileNames[iFile], 3, axes[1][iFile], doHold, Color, 'threads', 'columns', 'Writes/sec', scales[1], units[1], maxs[1]);\n"
    pycomm+="\n"
    pycomm+="  if naxes > 2 and axes[2] != None:\n"
    pycomm+="    makeSubPlot(fileNames[iFile], 4, axes[2][iFile], doHold, Color, 'threads', 'columns', 'Bytes/sec', scales[2], units[2], maxs[2]);\n"
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
    # Write the file names
    #------------------------------------------------------------
    
    pycomm+="files=$fileNames;\n"
    pycomm+="colors=['b', 'c', 'm', 'g', 'y', 'k'];\n"
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

    if [ "$labels" != \"\" ]; then

	pycomm+="nFile=np.shape(files)[0]\n"
	pycomm+="top=plt.rcParams['figure.subplot.top']\n"
	pycomm+="bottom=plt.rcParams['figure.subplot.bottom']\n"
	pycomm+="yrange=top-bottom\n"
	pycomm+="yint = yrange / nFile\n"
	pycomm+="\n"

	labarr=(`echo $labels`)
	labarrsize=${#labarr[@]}

	if [ $overplot == \""false\"" ] && [ $labarrsize -gt 1 ]; then
	    label=${labarr[$iter]//\"/}
	    iIter="0"
	    pyLabs=""
	    for label in $labels; do
		label=${label//\'/}
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
	    pycomm+="  plt.figtext(0.05, top - (i + 0.5)*yint, labels[i])\n"
	else
	    pycomm+="plt.figtext(0.05, 0.5, '${labels//\"/}')\n"
	fi  
    fi

    pycomm+="plt.axis('off')\n"
    pycomm+="plt.title('${title//\"/}')\n"
    
    if [ $overplot == \""true\"" ]; then
	pycomm+="axes = getSubplots(files, True)\n"
    else
        pycomm+="axes = getSubplots(files, False)\n"
    fi

    pycomm+="\n"

    if [ $plotwith == "true" ]; then
	pycomm+="plotFiles(files, plotwithfiles, axes, colors, scales, units, maxs)\n"
    else
	pycomm+="plotFiles(files, None, axes, colors, scales, units, maxs)\n"
    fi
    
    pycomm+="\n"

    echo "output = $output"
    if [ $output == \"\" ]; then
	pycomm+="plt.show();\n"
    else
	pycomm+="plt.savefig('${output//\"/}.png', format='png');\n"
    fi

    printf "$pycomm" | python
    printf "$pycomm" > /tmp/emltest.py
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
    stat=$(valOrDef stat 'ops' "$@")
    output=$(valOrDef output '' "$@")
	    
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
    generatePythonPlots "$1" $param1 $param2 $overplot $figsize "$labels" "$title" $scale $plotwith $output
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

batchRunCassSjb()
{
    cellsize=$(valOrDef cellsize '' "$@")
    cellsize=${cellsize//\"/}

    for cell in $cellsize
    do
	runCassSjb cellsize=$cell
    done
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
