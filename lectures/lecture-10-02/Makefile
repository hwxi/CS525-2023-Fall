test:: lambda1; ./lambda1
lambda1: lambda1.dats; \
patscc -DATS_MEMALLOC_LIBC -o $@ $<
clean:: ; rm -f *~
clean:: ; rm -f *_dats.c
cleanall:: clean
cleanall:: ; rm -f lambda1
