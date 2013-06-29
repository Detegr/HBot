SRC=HBot
CSRC=Config/config.c
PLUGINNAMES=Admin Wikla Random
PLUGINS=$(addprefix Plugin/,$(PLUGINNAMES))
PLUGINOBJECTS=$(addsuffix .o,$(PLUGINS))
COBJECTS=$(CSRC:.c=.o)

all: $(COBJECTS)
	ghc $(PLUGINS)
	ghc $(SRC) $(COBJECTS)
	# Dunno how to merge dependencies with ghc...
	ld -r Connection.o Config.o Parser.o Config/config.o Plugin/Admin.o -o Plugin/Admin.o.tmp
	#ld -r Plugin/Wikla.o -o Plugin/Wikla.o.tmp
	mv Plugin/Admin.o.tmp Plugin/Admin.o
	#mv Plugin/Wikla.o.tmp Plugin/Wikla.o

%.o: %.c %.h
	gcc -c -D_GNU_SOURCE -std=c99 $< -o $@

clean:
	-rm *.hi *.o Config/*.hi Config/*.o Plugin/*.hi Plugin/*.o
	-rm HBot

.PHONY: all clean
