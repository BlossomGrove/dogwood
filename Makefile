# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include vsn.mk
SUB_DIRECTORIES = src

include ../meadow/priv/Makefile.subdir

build:
	cd ../meadow; make
	cd ../circdb; make
	cd ../data_col; make
	cd ../jnets; make
	cd ../emqttc; make
	make
