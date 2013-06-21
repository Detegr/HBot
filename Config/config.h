#ifndef TAPI2P_CONFIG_H
#define TAPI2P_CONFIG_H

#include <stdio.h>
#define ITEM_MAXLEN 255

struct configitem
{
	char* key;
	char* val;
};

struct configsection
{
	char* name;
	unsigned int items;
	unsigned int size;
	struct configitem** item;
};

struct config
{
	unsigned int sections;
	unsigned int size;
	struct configsection** section;
};

struct configsection* config_find_section(struct config* haystack, const char* needle);
struct configitem* config_find_item(struct config* haystack, const char* needle, const char* section);
void config_init(struct config* conf);
void config_free(struct config* conf);
int config_load(struct config* conf, const char* filename);
void config_add(struct config* conf, const char* section, const char* key, const char* val);
int config_save(struct config* conf, const char* filename);
void config_flush(struct config* conf, FILE* stream);

#endif
