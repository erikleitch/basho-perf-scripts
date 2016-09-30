all:
	cd profiler; make
	cd riak_prof_tests; make
	if [ -d ../../tests ] ; then\
		cp riak_test_scripts/*.erl ../../tests;\
		cp riak_prof_tests/src/*.erl ../../tests;\
		cd ../../;make;\
	fi;
 
clean:
	cd profiler; make clean
	cd riak_prof_tests; make clean

