######
PATSCC=patscc
MYATSCC=myatscc
######
all::
all:: \
assign02_sol_dats
######
test:: \
assign02_sol_dats; ./$<
######
# assign02_sol_dats: \
# assign02_sol.dats; \
# $(MYATSCC) assign02_sol.dats
######
assign02_sol_dats: \
assign02_sol.dats; \
$(PATSCC) -O2 -DATS_MEMALLOC_GCBDW -o $@ $< -lgc
######
clean:: ; rm -f *~
######
cleanall:: clean
cleanall:: ; rm -f assign02_sol_dats
cleanall:: ; rm -f assign02_sol_dats.c
######

############ end of [Makefile] ############
