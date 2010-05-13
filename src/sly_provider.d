provider Sly {
	probe gc_alloc(int);
	probe gc_start();
	probe gc_end(int, int);
	probe gc_resize(int, int);
};

