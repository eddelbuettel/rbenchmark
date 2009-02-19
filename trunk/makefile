SRC = r

.PHONY: clean
clean:
	$(RM) $(addprefix ', $(addsuffix ', $(filter-out makefile $(addprefix %., $(SRC)), $(wildcard *))))
