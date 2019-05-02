
all: o

o: o.o
	gcc o.o -o o

o.o: o.ll
	llc -filetype=obj -relocation-model=pic o.ll
