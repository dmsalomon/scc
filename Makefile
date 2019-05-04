
all: o

run: o
	./o

o: o.o
	gcc o.o -o o

o.o: o.ll
	llc -filetype=obj -relocation-model=pic o.ll
	llc -filetype=asm -relocation-model=pic o.ll

clean:
	$(RM) o.o o o.ll o.llx o.s

.PHONY: clean
