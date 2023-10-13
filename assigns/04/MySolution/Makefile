######
PATSCC=patscc
MYATSCC=myatscc
######
all::
all:: \
assign04_sol_dats
######
test:: \
assign04_sol_dats; ./$<
######
# assign04_sol_dats: \
# assign04_sol.dats; \
# $(MYATSCC) assign04_sol.dats
######
assign04_sol_dats: \
assign04_sol.dats; \
$(PATSCC) -O2 -DATS_MEMALLOC_GCBDW -o $@ $< -lgc
######
clean:: ; rm -f *~
######
cleanall:: clean
cleanall:: ; rm -f assign04_sol_dats
cleanall:: ; rm -f assign04_sol_dats.c
######

############ end of [Makefile] ############
