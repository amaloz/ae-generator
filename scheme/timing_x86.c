#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include "ae.h"

#define M 15
#define N 64

#if __GNUC__
#define ALIGN(n)      __attribute__ ((aligned(n))) 
#elif _MSC_VER
#define ALIGN(n)      __declspec(align(n))
#else
#define ALIGN(n)
#endif

#if __INTEL_COMPILER
  #define STAMP ((unsigned)__rdtsc())
#elif (__GNUC__ && (__x86_64__ || __amd64__ || __i386__))
  #define STAMP ({unsigned res; __asm__ __volatile__ ("rdtsc" : "=a"(res) : : "edx"); res;})
#elif (_M_IX86)
  #include <intrin.h>
  #pragma intrinsic(__rdtsc)
  #define STAMP ((unsigned)__rdtsc())
#else
  #error -- Architechture not supported!
#endif


#define DO(x) do { \
int i; \
for (i = 0; i < M; i++) { \
unsigned c2, c1;\
x;x;\
c1 = STAMP;\
for (j = 0; j <= N; j++) { x; }\
c1 = STAMP - c1;\
x;x;\
c2 = STAMP;\
x;\
c2 = STAMP - c2;\
median_next(c1-c2);\
} } while (0)

unsigned values[M];
int num_values = 0;

extern char infoString[];  /* Each AE implementation must have a global one */

#ifndef MAX_ITER
#define MAX_ITER 1024
#endif

int comp(const void *x, const void *y) { return *(unsigned *)x - *(unsigned *)y; }
void median_next(unsigned x) { values[num_values++] = x; }
unsigned median_get(void) {
    unsigned res;
    /*for (res = 0; res < num_values; res++)
    //   printf("%d ", values[res]);
    //printf("\n");*/
    qsort(values, num_values, sizeof(unsigned), comp);
    res = values[num_values/2];
    num_values = 0;
    return res;
}
void median_print(void) {
    int res;
    qsort(values, num_values, sizeof(unsigned), comp);
    for (res = 0; res < num_values; res++)
       printf("%d ", values[res]);
    printf("\n");
}

extern void validate();

#define OCB_SCHEME 0
#define NEW_1_SCHEME 1
#define NEW_2_SCHEME 2
#define NEW_3_SCHEME 3

int run(int argc, char **argv, int scheme)
{
	/* Allocate locals */
	ALIGN(16) char pt[8*1024] = {0};
	ALIGN(16) char tag[16];
	ALIGN(16) unsigned char key[] = "abcdefghijklmnop";
	ALIGN(16) unsigned char nonce[] = "abcdefghijklmnop";
	char outbuf[MAX_ITER*15+1024];
	int iter_list[2048]; /* Populate w/ test lengths, -1 terminated */
	ae_ctx* ctx = ae_allocate(NULL);
	char *outp = outbuf;
	int i, j, len;
	double Hz;
	double ipi=0, tmpd;

	/* populate iter_list, terminate list with negative number */
	for (i=0; i<MAX_ITER; ++i)
		iter_list[i] = i+1;
	if (MAX_ITER < 44) iter_list[i++] = 44;
	if (MAX_ITER < 552) iter_list[i++] = 552;
	if (MAX_ITER < 576) iter_list[i++] = 576;
	if (MAX_ITER < 1500) iter_list[i++] = 1500;
	if (MAX_ITER < 4096) iter_list[i++] = 4096;
	iter_list[i] = -1;

    /* Create file for writing data */
	FILE *fp = NULL;
    char str_time[25];
	time_t tmp_time = time(NULL);
	struct tm *tp = localtime(&tmp_time);
	strftime(str_time, sizeof(str_time), "%F %R", tp);
	if ((argc < 2) || (argc > 3)) {
		printf("Usage: %s MHz [output_filename]\n", argv[0]);
		return 0;
	} else {
		Hz = 1e6 * strtol(argv[1], (char **)NULL, 10); (void)Hz;
		if (argc == 3)
			fp = fopen(argv[2], "w");
	}

    switch (scheme) {
    case OCB_SCHEME:
        outp += sprintf(outp, "OCB ");
        break;
    case NEW_1_SCHEME:
        outp += sprintf(outp, "NEW 1 ");
        break;
    case NEW_2_SCHEME:
        outp += sprintf(outp, "NEW 2 ");
        break;
    default:
        return 0;
    }

	
    outp += sprintf(outp, "%s ", infoString);
    #if __INTEL_COMPILER
        outp += sprintf(outp, "- Intel C %d.%d.%d ",
            (__ICC/100), ((__ICC/10)%10), (__ICC%10));
    #elif _MSC_VER
        outp += sprintf(outp, "- Microsoft C %d.%d ",
            (_MSC_VER/100), (_MSC_VER%100));
    #elif __clang_major__
        outp += sprintf(outp, "- Clang C %d.%d.%d ",
            __clang_major__, __clang_minor__, __clang_patchlevel__);
    #elif __clang__
        outp += sprintf(outp, "- Clang C 1.x ");
    #elif __GNUC__
        outp += sprintf(outp, "- GNU C %d.%d.%d ",
            __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
    #endif

    #if __x86_64__ || _M_X64
    outp += sprintf(outp, "x86_64 ");
    #elif __i386__ || _M_IX86
    outp += sprintf(outp, "x86_32 ");
    #elif __ARM_ARCH_7__ || __ARM_ARCH_7A__ || __ARM_ARCH_7R__ || __ARM_ARCH_7M__
    outp += sprintf(outp, "ARMv7 ");
    #elif __ARM__ || __ARMEL__
    outp += sprintf(outp, "ARMv5 ");
    #elif (__MIPS__ || __MIPSEL__) && __LP64__
    outp += sprintf(outp, "MIPS64 ");
    #elif __MIPS__ || __MIPSEL__
    outp += sprintf(outp, "MIPS32 ");
    #elif __ppc64__
    outp += sprintf(outp, "PPC64 ");
    #elif __ppc__
    outp += sprintf(outp, "PPC32 ");
    #elif __sparc__ && __LP64__
    outp += sprintf(outp, "SPARC64 ");
    #elif __sparc__
    outp += sprintf(outp, "SPARC32 ");
    #endif

    outp += sprintf(outp, "- Run %s\n\n",str_time);

	outp += sprintf(outp, "Context: %d bytes\n", ae_ctx_sizeof());
    DO(ae_init(ctx, key, 16, 12, 16));
    num_values = 0;
    DO(ae_init(ctx, key, 16, 12, 16));
    outp += sprintf(outp, "Key setup: %d cycles\n\n", (int)((median_get())/(double)N));
        
	/*
	 * Get times over different lengths
	 */
    switch (scheme){
    case OCB_SCHEME:
        outp += sprintf(outp, "OCB\n");
        i=0;
        len = iter_list[0];
        while (len >= 0) {
            nonce[11] = 0;
            if (len % 32 == 0) {
                DO(ae_encrypt(ctx, nonce, pt, len, NULL, 0, pt, tag, 1); nonce[11] += 1);
                tmpd = ((median_get())/(len*(double)N));
                outp += sprintf(outp, "%5d  %6.2f\n", len, tmpd);
            }
            ++i;
            len = iter_list[i];
        }
        break;
    case NEW_1_SCHEME:
        outp += sprintf(outp, "New 1 Scheme\n");
        i=0;
        len = iter_list[0];
        while (len >= 0) {
            nonce[11] = 0;
            if (len % 32 == 0) {
                DO(new_1_ae_encrypt(ctx, nonce, pt, len, NULL, 0, pt, tag, 1); nonce[11] += 1);
                tmpd = ((median_get())/(len*(double)N));
                outp += sprintf(outp, "%5d  %6.2f\n", len, tmpd);
            }
            ++i;
            len = iter_list[i];
        }
        break;
    case NEW_2_SCHEME:
        outp += sprintf(outp, "New 2 Scheme\n");
        i=0;
        len = iter_list[0];
        while (len >= 0) {
            nonce[11] = 0;
            if (len % 32 == 0) {
                DO(new_2_ae_encrypt(ctx, nonce, pt, len, NULL, 0, pt, tag, 1); nonce[11] += 1);
                tmpd = ((median_get())/(len*(double)N));
                outp += sprintf(outp, "%5d  %6.2f\n", len, tmpd);
            }
            ++i;
            len = iter_list[i];
        }
        break;
    default:
        assert(0);
    }

	if (fp) {
        fprintf(fp, "%s", outbuf);
        fclose(fp);
    } else
        fprintf(stdout, "%s", outbuf);
    
	return ((pt[0]==12) && (pt[10]==34) && (pt[20]==56) && (pt[30]==78));
}

int main(int argc, char **argv) {
    run(argc, argv, OCB_SCHEME);
}
