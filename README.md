# Synopsis

basho-perf-scripts is a collection of bash scripts for use with
basho-perf. In particular, bash_scripts/basho_perf_scripts.sh provides scripts to parse output log files from
basho-perf runs and plot summary statistics, via calls to the python matplotlib library.

# Usage

* <a href=#surface_plots>Surface Plots</a>
* <a href=#dynamic_ring_analyzer>Dynamic Ring Analysis</a>

<a name="surface_plots">Surface Plots</a>

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

