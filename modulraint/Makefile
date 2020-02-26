SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =

all: modularint.o
check: testRunner

testRunner: MODULAR_INT.sig MODULAR_INT.smi ../array31/MINIMUM_ARRAY.sig \
 ../array31/MINIMUM_ARRAY.smi ../array31/array31.smi modularint.smi \
 modularint_test.smi ../array31/array31.o modularint.o modularint_test.o
	$(SMLSHARP) $(LDFLAGS) -o testRunner modularint_test.smi $(LIBS)
../array31/array31.o: ../array31/array31.sml ../array31/MINIMUM_ARRAY.sig \
 ../array31/MINIMUM_ARRAY.smi ../array31/array31.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../array31/array31.o -c \
 ../array31/array31.sml
modularint.o: modularint.sml MODULAR_INT.sig MODULAR_INT.smi \
 ../array31/MINIMUM_ARRAY.sig ../array31/MINIMUM_ARRAY.smi \
 ../array31/array31.smi modularint.smi
	$(SMLSHARP) $(SMLFLAGS) -o modularint.o -c modularint.sml
modularint_test.o: modularint_test.sml MODULAR_INT.sig MODULAR_INT.smi \
 ../array31/MINIMUM_ARRAY.sig ../array31/MINIMUM_ARRAY.smi \
 ../array31/array31.smi modularint.smi modularint_test.smi
	$(SMLSHARP) $(SMLFLAGS) -o modularint_test.o -c modularint_test.sml

clean:
	rm -f modularint.o modularint_test.o testRunner
