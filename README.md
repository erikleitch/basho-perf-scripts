# Synopsis

basho-perf-scripts is a collection of bash, erlang and python scripts for use with
basho-perf and riak_test.

For basho-perf, bash_scripts/basho_perf_scripts.sh provides scripts to
parse various output files from basho-perf runs and make summary
plots, via calls to the python matplotlib library.

# Usage

* <a href=#general>General</a>
* <a href=#surface_plots>Surface Plots</a>
* <a href=#dynamic_ring_analyzer>Dynamic Ring Analysis</a>
* <a href=#static_ring_analyzer>Static Ring Analysis</a>

<hr>
<a name="general">**General**</a>

In general, `basho-perf-scripts` is meant to be used from your
basho-perf (or riak_test) directory, so best to check it out and run
it from there.

To make all commands in `basho-perf-scripts` available in bash, do:

```
source basho-perf-scripts/prof_source
```

<hr>
<a name="surface_plots">**Surface Plots**</a>

Suppose you have a basho-perf run script like:

```
MacBook-Pro.local:basho-perf:>cat riak-sjb_10.run

test = "/Users/eml/projects/riak/utilities/internal_utilities_checkout/basho-perf/lib/tests/riak-simple_java_bench";
hosts = "/Users/eml/projects/riak/utilities/internal_utilities_checkout/etc/hosts.d/softlayer-b";

simple_java_bench = {
        threads = [32, 64, 128, 256];
        columns = [1, 5, 10, 15];
        cell_size = 10;
}
			
```

And you capture the output to a log file, i.e.:

```
basho-perf run riak-sjb.run &> riak-sjb_10.log
```

Then the following command:

```
source basho-perf-scripts/bash_scripts/basho_perf_scripts.sh

plotlogfile riak-sjb_10.log threads columns
```

Produces this image:

![alt tag](https://github.com/erikleitch/basho-perf-scripts/blob/master/images/example1.png)

If a cellsize is specified, the script will additionally plot bytes/sec:

```
plotlogfile riak-sjb_10.log threads columns cellsize=10
```

Produces this image:

![alt tag](https://github.com/erikleitch/basho-perf-scripts/blob/master/images/example2.png)

Multiple files can also be plotted:

```
plotlogfile "riak-sjb_10.log riak-sjb_100.log" threads columns cellsize="10 100"
```

Produces this image:

![alt tag](https://github.com/erikleitch/basho-perf-scripts/blob/master/images/example3.png)

or

![alt tag](https://github.com/erikleitch/basho-perf-scripts/blob/master/images/example4.png)

if ```overplot=true``` is specified.

<hr>
<a name="dynamic_ring_analyzer">**Dynamic Ring Analysis**</a>

These tools are meant to be used with a special branch of riak_ee: `perf/riak_ts_analyzer`.

Suppose you've run a basho-perf test with the following config file:

```
test = "riak_ts-ycsb";
hosts = "softlayer-dev-a";

riak_ts = {
        n_val = 1
	branch=perf/riak_ts_analyzer
	}

YCSB = {
        riak.r_val=1
	riak.w_val=1

        readproportion=0
	updateproportion=0
	scanproportion=0
	insertproportion=1

        fieldcount=10
	fieldlength=1

        threadcount=8

        maxexecutiontime=60
	}
```

By default, this branch logs time-resolved counters to a file (on the softlayer clusters, these end up as `/tmp/riak_atomicCounters.txt`).

Running the command `retrieveAnalyzerFiles softlayer-dev-a` will fetch and rename the log files locally.

Running the command `animate` will display a movie of those logs (see
`basho-perf-scripts/bash_scripts/basho-perf-scripts.sh` for details of
how to configure the underlying python script), a screen capture of which is shown below:

<center>
![alt tag](https://github.com/erikleitch/basho-perf-scripts/blob/master/images/ycsb_frame.png)
</center>

Each inner ring represents a separate physical node, and partitions
are highlighted in the movie, with brightness proportional to the rate
of operations against those partitions (relative to the maximum
instantaneous rate of any partition).  The outermost ring shows the
cumulative operations against every partition in the ring, summed over
all nodes.

<hr>
<a name="static_ring_analyzer">**Static Ring Analysis**</a>

The `basho-perf-scripts` repo also contains functions for visualizing
the static data distribution of a riak cluster.

Suppose you have just run a test on `softlayer-dev-a`.  To visualize the data distribution, you can run the commands:

```
buildPartitionFiles softlayer-dev-a
python basho-perf-scripts/python_scripts/makeringplot.py file=ring.txt
```

The first iterates over all leveldb instances on each riak node,
building a file that represents the data distribution for that node,
then concatenates the results to a single file (ring.txt), which is
then displayed by the python script, producing an image like:

<center>
![alt tag](https://github.com/erikleitch/basho-perf-scripts/blob/master/images/static_ring.png)
</center>

Similar to the dynamic ring plots, each inner ring represents a
separate physical node, and partitions containing data are highlighted
with brightness proportional to the number of keys in those partitions
(relative to the total number of keys).  The outermost ring shows the
cumulative content (number of keys) in every partition in the ring,
summed over all nodes.
