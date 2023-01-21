#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

struct List {
	char* str;
	stuct List *next;
}
typedef struct List List;

List* liste_vide(){
	return NULL;
}

List* cons(char* str, List* list){
	List* newCell = malloc(sizeof(List));
	newCell->str = str;
	newCell->next = list;
	return newCell;
}

List* merge(List* list1, List* list2){
	if(list1 == NULL){
		return list2;
	}else if(list2 == NULL){
		return list1;
	}else{
		List* last = list1;
		while(last->next != NULL){
			last = last->next;
		}
		last->next = list2;
		return list1;
	}
	assert(false);
	return NULL;
}

char* head(List* list){
	assert(list != NULL);
	return list->value;
}

