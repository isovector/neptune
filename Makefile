all:
	moonc scripts/*.moon &
	stack build -j 4 --fast
	stack exec neptune

.PHONY: all

