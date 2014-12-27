PROJECT=py

all: compile python

include resources/make/common.mk
include resources/make/dist.mk
include resources/make/python.mk
