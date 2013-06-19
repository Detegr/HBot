#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

static int binarysearch(const void* arr, const void* key, size_t elemsize, unsigned int max, int(*cmp)(const void*, const void*));
static int item_compare(const void* a, const void* b);
static int section_compare(const void* a, const void* b);
static int section_sort(const void* a, const void* b);

static void item_add(struct configsection* section, const char* key, const char* val);
static int item_find(struct configsection* haystack, const char* needle);
static void item_free(struct configitem* item);

static void section_new(struct config* conf, const char* name);
static void section_free(struct configsection* sect);

static int item_compare(const void* a, const void* b)
{
	return strncmp((const char*)a,(*(struct configitem**)b)->key,ITEM_MAXLEN);
}
static int item_sort(const void* a, const void* b)
{
	return strncmp((*(struct configitem**)a)->key,(*(struct configitem**)b)->key,ITEM_MAXLEN);
}

static int section_compare(const void* a, const void* b)
{
	return strncmp((const char*)a,(*(struct configsection**)b)->name,ITEM_MAXLEN);
}

static int section_sort(const void* a, const void* b)
{
	return strncmp((*(struct configsection**)a)->name,(*(struct configsection**)b)->name,ITEM_MAXLEN);
}

static void item_free(struct configitem* item)
{
	free(item->key);
	if(item->val) free(item->val);
}

static void section_free(struct configsection* sect)
{
	free(sect->name);
	for(unsigned int i=0; i<sect->items; ++i)
	{
		item_free(sect->item[i]);
		free(sect->item[i]);
	}
	free(sect->item);
}

void config_free(struct config* conf)
{
	for(unsigned int i=0; i<conf->sections; ++i)
	{
		section_free(conf->section[i]);
		free(conf->section[i]);
	}
	free(conf->section);
}

static int item_find(struct configsection* haystack, const char* needle)
{
	if(haystack->items)
	{
		return binarysearch(haystack->item, needle, sizeof(struct configitem*), haystack->items-1, item_compare);
	}
	return -1;
}

void config_init(struct config* conf)
{
	conf->sections = 0;
	conf->size = 0;
	conf->section=NULL;
}

static void item_add(struct configsection* section, const char* key, const char* val)
{
	/* Allocate more space if needed */
	if(section->items >= section->size)
	{
		section->size = section->size?section->size*2:8;
		struct configitem** newitems = realloc(section->item, section->size * sizeof(struct configitem*));
		assert(newitems != NULL);
		section->item=newitems;
	}
	/* Assign new item */
	struct configitem* newitem = (struct configitem*)malloc(sizeof(struct configitem));
	newitem->key = strdup(key);
	if(val) newitem->val = strdup(val);
	else newitem->val = NULL;
	section->item[section->items++] = newitem;

	qsort(section->item, section->items, sizeof(struct configitem*), item_sort);
}

void config_add(struct config* conf, const char* section, const char* key, const char* val)
{
	struct configsection* sect;
	if((sect=config_find_section(conf, section)))
	{
		struct configitem* item;
		if((item=config_find_item(conf, key, section)))
		{
			if(item->val) free(item->val);
			if(val) item->val=strdup(val);
			else item->val=NULL;
		}
		else item_add(sect, key, val);
	}
	else
	{
		section_new(conf, section);
		config_add(conf, section, key, val);
	}
}

static void section_new(struct config* conf, const char* name)
{
	if(conf->sections >= conf->size)
	{
		conf->size = conf->size?conf->size*2:8;
		struct configsection** newsection = (struct configsection**)realloc(conf->section, conf->size * sizeof(struct configsection*));
		assert(newsection != NULL);
		conf->section=newsection;
	}
	/* Assign new item */
	struct configsection* newsection = (struct configsection*)malloc(sizeof(struct configsection));
	newsection->name = strdup(name);
	newsection->items = 0;
	newsection->size = 0;
	newsection->item = NULL;
	conf->section[conf->sections++] = newsection;

	qsort(conf->section, conf->sections, sizeof(struct configsection*), section_sort);
}

static int binarysearch(const void* arr, const void* key, size_t elemsize, unsigned int max, int(*cmp)(const void*, const void*))
{
	unsigned int min=0;
	while(min<max)
	{
		unsigned int mid = (min+max)>>1;
		assert(mid<max);
		if(cmp(key,arr+(mid*elemsize)) > 0) min=mid+1;
		else max=mid;
	}
	if((max==min) && (cmp(key,arr+(min*elemsize)) == 0)) return min;
	else return -1;
}

struct configsection* config_find_section(struct config* haystack, const char* needle)
{
	if(haystack->sections)
	{
		int i = binarysearch(haystack->section, needle, sizeof(struct configsection*), haystack->sections-1, section_compare);
		if(i>=0) return haystack->section[i];
	}
	return NULL;
}

struct configitem* config_find_item(struct config* haystack, const char* needle, const char* section)
{
	if(section)
	{
		struct configsection* sect;
		if((sect=config_find_section(haystack, section)))
		{
			int item=item_find(sect, needle);
			if(item != -1) return sect->item[item];
		}
	}
	else
	{
		for(unsigned int i=0; i<haystack->sections; ++i)
		{
			int item=item_find(haystack->section[i], needle);
			if(item != -1) return haystack->section[i]->item[item];
		}
	}
	return NULL;
}

void config_flush(struct config* conf, FILE* stream)
{
	for(unsigned int i=0; i<conf->sections; ++i)
	{
		fprintf(stream, "[%s]\n", conf->section[i]->name);
		for(unsigned int j=0; j<conf->section[i]->items; ++j)
		{
			struct configitem* item = conf->section[i]->item[j];
			if(item->val)
			{
				fprintf(stream, "\t%s=%s\n", item->key, item->val);
			}
			else fprintf(stream, "\t%s\n", item->key);
		}
	}
}

int config_load(struct config* conf, const char* filename)
{
	config_init(conf);
	FILE* f=fopen(filename, "r");
	if(!f) return -1;
	int r=1;
	char* line=NULL;
	size_t s=0;
	char header[ITEM_MAXLEN];
	char left[ITEM_MAXLEN];
	char right[ITEM_MAXLEN];
	while((r = getline(&line, &s, f)) != -1)
	{
		int h=sscanf(line, "[%[^\n]", header);
		if(!h)
		{
			h=sscanf(line, "%*[ \t]%[^=]=%[^\n]", left, right);
			if(h==1)
			{
				h=sscanf(line, "%*[ \t]%[^\n]", left);
				if(h==1) config_add(conf, header, left, NULL);
				else return -1;
			}
			else if(h==2) config_add(conf, header, left, right);
			else return -1;
		}
		else
		{
			size_t hlen = strnlen(header, ITEM_MAXLEN)-1;
			if(header[hlen]==']') header[hlen]=0;
			else return -1;
		}
	}
	free(line);
	fclose(f);
	return 0;
}
