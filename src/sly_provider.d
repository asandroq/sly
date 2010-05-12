provider Sly {
	probe gc_allocd(int);
	probe gc_start();
	probe gc_end(int, int);
};

