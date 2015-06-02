.PHONY: all clean

all: a.out

a.out: timing_x86.c ocb.c
	gcc -O3 -march=native timing_x86.c ocb.c

clean:
	rm -rf a.out
