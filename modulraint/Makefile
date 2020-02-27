SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =

all: memoizedmodularint.o modularint.o
check: testRunner

testRunner: MEMOIZED_MODULAR_INT.sig memoizedmodularint.smi \
 memoizedmodularint_test.smi MODULAR_INT.sig modularint.smi \
 modularint_test.smi main_test.smi memoizedmodularint.o \
 memoizedmodularint_test.o modularint.o modularint_test.o main_test.o
	$(SMLSHARP) $(LDFLAGS) -o testRunner main_test.smi $(LIBS)
main_test.o: main_test.sml memoizedmodularint.smi memoizedmodularint_test.smi \
 modularint.smi modularint_test.smi main_test.smi
	$(SMLSHARP) $(SMLFLAGS) -o main_test.o -c main_test.sml

memoizedmodularint.o: memoizedmodularint.sml MEMOIZED_MODULAR_INT.sig \
 memoizedmodularint.smi
	$(SMLSHARP) $(SMLFLAGS) -o memoizedmodularint.o -c \
 memoizedmodularint.sml
memoizedmodularint_test.o: memoizedmodularint_test.sml memoizedmodularint.smi \
 memoizedmodularint_test.smi
	$(SMLSHARP) $(SMLFLAGS) -o memoizedmodularint_test.o -c \
 memoizedmodularint_test.sml

modularint.o: modularint.sml MODULAR_INT.sig modularint.smi
	$(SMLSHARP) $(SMLFLAGS) -o modularint.o -c modularint.sml
modularint_test.o: modularint_test.sml modularint.smi modularint_test.smi
	$(SMLSHARP) $(SMLFLAGS) -o modularint_test.o -c modularint_test.sml

clean:
	rm -f memoizedmodularint.o memoizedmodularint_test.o \
          modularint.o modularint_test.o \
          main_test.o testRunner
