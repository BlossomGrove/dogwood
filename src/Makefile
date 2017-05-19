include ../vsn.mk

# ----------------------------------------------------
# VARIABLES
# ----------------------------------------------------

APP_NAME = dogwood

MODULES= \
	dogwood \
	dogwood_app \
	dogwood_sup \
	dogwood_manager \
	dogwood_access \
	dogwood_db \
	dogwood_lib \
	mod_dogwood

TARGET_FILES = $(APP_TARGET) $(EMAKE_TARGET)

include ../../meadow/priv/Makefile.vars

# ----------------------------------------------------
# BUILD TARGETS
# ----------------------------------------------------
all: $(TARGET_FILES)
	erl -make

clean:
	rm -f $(APP_TARGET) $(EMAKE_TARGET) ../ebin/*.beam

doc:

include ../../meadow/priv/Makefile.rules

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
