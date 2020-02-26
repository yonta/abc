SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =

all: array31.o
check: testRunner

testRunner: MINIMUM_ARRAY.sig MINIMUM_ARRAY.smi array31.smi array31_test.smi \
 array31.o array31_test.o
	$(SMLSHARP) $(LDFLAGS) -o testRunner array31_test.smi $(LIBS)
array31.o: array31.sml MINIMUM_ARRAY.sig MINIMUM_ARRAY.smi array31.smi
	$(SMLSHARP) $(SMLFLAGS) -o array31.o -c array31.sml
array31_test.o: array31_test.sml MINIMUM_ARRAY.sig MINIMUM_ARRAY.smi \
 array31.smi array31_test.smi
	$(SMLSHARP) $(SMLFLAGS) -o array31_test.o -c array31_test.sml

clean:
	rm -f array31.o array31_test.o testRunner
