SRC=HBot
CSRC=Config/config.c
PLUGINNAMES=Admin Wikla
PLUGINS=$(addprefix Plugin/,$(PLUGINNAMES))
PLUGINOBJECTS=$(addsuffix .o,$(PLUGINS))
COBJECTS=$(CSRC:.c=.o)

all: $(COBJECTS)
	ghc $(PLUGINS)
	ghc $(SRC) $(COBJECTS)
	# Dunno how to merge dependencies with ghc...
	ld -r Config.o Parser.o Config/config.o Plugin/Admin.o -o Plugin/Adminn.o
	mv Plugin/Adminn.o Plugin/Admin.o

%.o: %.c %.h
	gcc -c -D_GNU_SOURCE -std=c99 $< -o $@

clean:
	-rm *.hi *.o Config/*.hi Config/*.o Plugin/*.hi Plugin/*.o
	-rm HBot

.PHONY: all clean
