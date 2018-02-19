all:
	moonc scripts/*.moon &
	moonc scripts/rooms/*.moon &
	stack build -j 4 --fast
	stack exec neptune

.PHONY: all

