import json
import urllib2
import threading
import random
import string
import collections
import itertools
import sys

thread_count = 2

def write_op(url, data):
	req = urllib2.Request(url)
	req.add_header('Content-Type', 'application/json')
	response = urllib2.urlopen(req, data)

def load_worker(thread_id, max_workers, file_path):
	with open(file_path, 'r') as f:
		nthlines = itertools.islice(f, thread_id, None, max_workers)
		for line in nthlines:
			url = line.split(";")[0][1:-1]
			data = line.split(";")[1].replace("\\\"", "\"").replace("\\\\", "\\")[1:-2]
			write_op(url, data)

	return

threads = []

file_path = sys.argv[1]
print "Replaying from %s" % file_path

for thread_index in range(thread_count):
	t = threading.Thread(target=load_worker, args=(thread_index,thread_count,file_path))
	threads.append(t)
	t.start()
