######
PATSCC=patscc
MYATSCC=myatscc
######
all::
all:: \
lecture-09-18_dats
######
# lecture-09-18_dats: \
# lecture-09-18.dats; \
# $(MYATSCC) lecture-09-18.dats
######
lecture-09-18_dats: \
lecture-09-18.dats; \
$(PATSCC) -o $@ -DATS_MEMALLOC_LIBC $<
######
clean:: ; rm -f *~
######
cleanall:: clean
cleanall:: ; rm -f lecture-09-18_dats
######

###### end of [Makefile] ######
