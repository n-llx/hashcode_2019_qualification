#include <stdin.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>

struct Picture {
	int id;
	int nbTags;
	List* tags;
	bool h;
};
typedef struct Picture Picture;

