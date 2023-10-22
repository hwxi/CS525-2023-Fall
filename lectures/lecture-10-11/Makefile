test:: lambda2; ./lambda2
lambda2: lambda2.dats; \
patscc -DATS_MEMALLOC_GCBDW -o $@ $< -lgc
clean:: ; rm -f *~
clean:: ; rm -f *_dats.c
cleanall:: clean
cleanall:: ; rm -f lambda2
