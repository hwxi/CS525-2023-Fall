######
PATSCC=patscc
MYATSCC=myatscc
######
all::
all:: \
assign01_sol_dats
######
# assign01_sol_dats: \
# assign01_sol.dats; \
# $(MYATSCC) assign01_sol.dats
######
assign01_sol_dats: \
assign01_sol.dats; \
$(PATSCC) -DATS_MEMALLOC_LIBC -o $@ $<
######
clean:: ; rm -f *~
######
cleanall:: clean
cleanall:: ; rm -f assign01_sol_dats
cleanall:: ; rm -f assign01_sol_dats.c
######

###### end of [Makefile] ######
