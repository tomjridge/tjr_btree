The documentation is in the n_doc/ subdirectory.

See `m_main/` for examples.

# Example with `int -> int` map

`./ii_example.native` is a reasonable demonstration:

~~~
$ time ./ii_example.native 
Executing 10000 writes...
Writing...
Deleting...
Checking...
Full check...

real	0m1.120s
user	0m0.908s
sys	0m0.188s
~~~

# Example with `string -> string` map

Reading and writing 10k (short) strings:

~~~
$ time ./simple_example.native 
Executing 10000 writes...
Writing...
Deleting...
Checking...
Full check...

real	0m1.726s
user	0m1.252s
sys	0m0.452s
~~~