
all: o

run: o
	./o

o: o.o
	gcc o.o -o o

o.o: o.ll
	llc -O3 -filetype=obj -relocation-model=pic o.ll
	llc -O3 -filetype=asm -relocation-model=pic o.ll

P ?= ""
o.ll: $(P)
	@[ -n $(P) ] && python gen.py $(P) || (echo "need o.ll" && false)

clean:
	$(RM) o.o o o.ll o.llx o.s

.PHONY: clean
