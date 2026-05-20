SIF ?= $(GROUP_HOME)/containers/upcoding-sim_4.5.3.sif

.PHONY: container clean

container: $(SIF)

$(SIF): apptainer.def renv.lock $(wildcard renv/cellar/*.tar.gz)
	mkdir -p $(dir $@)
	apptainer build --fakeroot $@ apptainer.def

clean:
	rm -f $(SIF)
