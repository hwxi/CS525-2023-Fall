######
PATSCC=patscc
MYATSCC=myatscc
######
all::
all:: \
midterm_main_dats
######
test:: \
midterm_main_dats; ./$<
######
# midterm_main_dats: \
# midterm_main.dats; \
# $(MYATSCC) midterm_main.dats
######
midterm_main_dats: \
midterm_main.dats; \
$(PATSCC) -O2 -D_GNU_SOURCE -DATS_MEMALLOC_GCBDW -o $@ $< -lgc
######
clean:: ; rm -f *~
######
cleanall:: clean
cleanall:: ; rm -f midterm_main_dats
cleanall:: ; rm -f midterm_main_dats.c
######

############ end of [Makefile] ############
