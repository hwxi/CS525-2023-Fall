######
PATSCC=patscc
MYATSCC=myatscc
######
all::
all:: \
midterm_sol_dats
######
test:: \
midterm_sol_dats; ./$<
######
# midterm_sol_dats: \
# midterm_sol.dats; \
# $(MYATSCC) midterm_sol.dats
######
midterm_sol_dats: \
midterm_sol.dats; \
$(PATSCC) -O2 -D_GNU_SOURCE -DATS_MEMALLOC_GCBDW -o $@ $< -lgc
######
clean:: ; rm -f *~
######
cleanall:: clean
cleanall:: ; rm -f midterm_sol_dats
cleanall:: ; rm -f midterm_sol_dats.c
######

############ end of [Makefile] ############
