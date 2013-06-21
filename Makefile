SRC=HBot Config/Config
CSRC=Config/config.c
PLUGINNAMES=Admin Wikla
PLUGINS=$(addprefix Plugin/,$(PLUGINNAMES))
COBJECTS=$(CSRC:.c=.o)

all: $(COBJECTS)
	ghc $(PLUGINS) -iConfig
	ghc $(SRC) $(COBJECTS)

%.o: %.c %.h
	gcc -c -D_GNU_SOURCE -std=c99 $< -o $@

clean:
	-rm *.hi *.o Config/*.hi Config/*.o Plugin/*.hi Plugin/*.o
	-rm HBot

.PHONY: all clean
