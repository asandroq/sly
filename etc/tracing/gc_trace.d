
Sly$target:::gc_end
{
   printf("from %i to %i", arg0, arg1);
}

Sly$target:::gc_resize
{
   printf("old: %i new: %i", arg0, arg1);
}

