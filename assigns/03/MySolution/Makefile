######
PATSCC=patscc
MYATSCC=myatscc
######
all::
all:: \
assign03_sol_dats
######
test:: \
assign03_sol_dats; ./$<
######
# assign03_sol_dats: \
# assign03_sol.dats; \
# $(MYATSCC) assign03_sol.dats
######
assign03_sol_dats: \
assign03_sol.dats; \
$(PATSCC) -O2 -DATS_MEMALLOC_GCBDW -o $@ $< -lgc
######
clean:: ; rm -f *~
######
cleanall:: clean
cleanall:: ; rm -f assign03_sol_dats
cleanall:: ; rm -f assign03_sol_dats.c
######

############ end of [Makefile] ############
