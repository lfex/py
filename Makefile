PROJECT=py

all: compile python

include resources/make/common.mk
include resources/make/e2.mk
include resources/make/python.mk
