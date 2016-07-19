# Synopsis

basho-perf-scripts is a collection of bash scripts for use with
basho-perf.  The primary function is to parse output log files from
basho-perf runs and plot summary statistics.

# Usage

Suppose you have a basho-perf run script like:

```
MacBook-Pro.local:basho-perf:>cat riak-sjb.run

test = "/Users/eml/projects/riak/utilities/internal_utilities_checkout/basho-perf/lib/tests/riak-simple_java_bench";
hosts = "/Users/eml/projects/riak/utilities/internal_utilities_checkout/etc/hosts.d/softlayer-b";

simple_java_bench = {
        threads = [32, 64, 128, 256];
        columns = [1, 5, 10, 15];
        cell_size = 10;
}
			
```

And you capture the output to a log file, ie:

```
basho-perf run riak-sjb.run &> riak-sjb.log

source basho-perf-scripts/bash_scripts/basho_perf_scripts.sh
plotlogfile riak-sjb.log
```

Produces the following output:

![alt tag](https://github.com/erikleitch/basho-perf-scripts/blob/master/images/example1.png)
