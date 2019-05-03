
all: o

o: o.o
	gcc o.o -o o

o.o: o.ll
	llc -O3 -filetype=obj -relocation-model=pic o.ll
	llc -O3 -filetype=asm -relocation-model=pic o.ll

clean:
	$(RM) o.o o o.ll o.llx o.s

.PHONY: clean
