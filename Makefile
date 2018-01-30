all:
	moonc scripts/*.moon &
	stack build
	stack exec neptune

.PHONY: all

