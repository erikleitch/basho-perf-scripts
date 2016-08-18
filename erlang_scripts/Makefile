all:
	cd profiler; make
	cd riak_prof_tests; make
	if [ -d ../../tests ] ; then cp riak_test_scripts/*.erl ../../tests; fi;
 
clean:
	cd profiler; make clean
	cd riak_prof_tests; make clean

