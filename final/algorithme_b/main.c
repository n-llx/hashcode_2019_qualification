#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>


struct image_t {
  int id;
  int nb_tags;
  char** tags;
};
typedef struct image_t image_t;


int compareString (const void* a, const void* b){
  return strcmp(*(const char**)a, *(const char**)b);
}

void triTags(image_t* image){
  qsort(image->tags, image->nb_tags, sizeof(char*), compareString);
}

void printStringArray(int n, char** array){
  for(int i = 0; i < n; i++){
    printf("%s ", array[i]);
  }
  printf("\n");
}

int max(int a, int b){
  return(a < b ? b : a);
}

int min(int a, int b){
  return(a < b ? a : b);
}

void printTags(image_t* i){
  printStringArray(i->nb_tags, i->tags);
}

int scoreImage(image_t* i1, image_t* i2){
  assert(i1 != NULL);
  assert(i2 != NULL);
  triTags(i1);
  triTags(i2);
  int in1ButNotIn2 = 0;
  int in2ButNotIn1 = 0;
  int inBoth = 0;
  int indiceParcours1 = 0;
  int indiceParcours2 = 0;
  while(indiceParcours1 != i1->nb_tags && indiceParcours2 != i2->nb_tags){
    int equal = strcmp(i1->tags[indiceParcours1], i2->tags[indiceParcours2]);
    if(equal == 0){
      inBoth++;
      indiceParcours1++;
      indiceParcours2++;
    }else if(equal < 0){
      in1ButNotIn2++;
      indiceParcours1++;
    }else if(equal > 0){
      in2ButNotIn1++;
      indiceParcours2++;
    }
  }
  if(indiceParcours1 == i1->nb_tags){
    in2ButNotIn1 += i2->nb_tags - indiceParcours2;
  }else{
    in1ButNotIn2 += i1->nb_tags - indiceParcours1;
  }
  return (min(min(in1ButNotIn2,in2ButNotIn1), inBoth));
}

bool* initArrayBool(int length){
  bool* res = malloc(sizeof(bool) * length);
  for(int i = 0; i < length; i++){
    res[i] = false;
  }
  return res;
}

image_t** initArrayImage(int length){
  image_t** res = malloc(sizeof(image_t*) * length);
  for(int i = 0; i < length; i++){
    res[i] = malloc(sizeof(image_t));
    res[i]->id = -1;
    res[i]->nb_tags = -1;
    res[i]->tags = NULL;
  }
  return res;
}

char** initArrayString(int length, int maxString){
  char** res = malloc(sizeof(char*) * length);
  for(int i = 0; i < length; i++){
    res[i] = malloc(sizeof(char) * maxString);
  }
  return res;
}


void bufferForward(FILE* f, int nbChar){
  for(int i = 0; i < nbChar; i++){
    fgetc(f);
  }
}

image_t** readEntry(int* length, char* nameFile){
  FILE* f = fopen(nameFile, "r");
  assert(f != NULL);
  fscanf(f, "%d\n", length);
  image_t** arrayImages = initArrayImage(*length);
  for(int i = 0; i < *length; i++){
    bufferForward(f, 2);
    int nbTags = -1;
    fscanf(f, "%d ", &nbTags);
    char** arrayTags = initArrayString(nbTags, 100);
    for(int j = 0; j < nbTags; j++){
      fscanf(f,"%s ", arrayTags[j]);
    }
    arrayImages[i]->id = i;
    arrayImages[i]->nb_tags = nbTags;
    arrayImages[i]->tags = arrayTags;
  }
  fclose(f);
  return arrayImages;
}

void swap(image_t** allImages, int i, int j){
  image_t* temp = allImages[i];
  allImages[i] = allImages[j];
  allImages[j] = temp;
}

void findBestFit(image_t** allImages, int nbImage, int position){
  assert(position < nbImage - 1);
  //Ne pas appeler sur le dernier element de allImages
  
  int maxScore = -1;
  int idMaxScore = -1;
  for(int i = position + 1; i < nbImage; i++){
    int score = scoreImage(allImages[position], allImages[i]);
    if(score > maxScore){
      maxScore = score;
      idMaxScore = i;
    }
  }
  assert(idMaxScore >= 0);
  swap(allImages, position + 1, idMaxScore);
}

void algoGlouton(image_t** allImages, int nbImage){
  for(int i = 0; i < nbImage - 1; i++){
    printf("\033[F");
    printf("\033[K");
    printf("%d / 80000\n", i);
    findBestFit(allImages, nbImage, i);
  }
}

void writeResult(char* nameOfFile, image_t** result, int nbImage){
  FILE* f = fopen(nameOfFile, "w");
  fprintf(f,"%d\n", nbImage);
  for(int i = 0; i < nbImage; i++){
    fprintf(f,"%d\n", result[i]->id);
  }
  fclose(f);
}

int main(int argc, char* argv[]){
  assert(argc > 2);
  int nbImage = 0;
  image_t** arrayImages = readEntry(&nbImage, argv[1]);
  algoGlouton(arrayImages, nbImage);  
  writeResult(argv[2], arrayImages, nbImage);



  // char* tableau1[] = {"tag1", "tag3", "tag2", "tag4"};
  // char* tableau2[] = {"tag1", "tag8", "tag9", "tag2"};
  // image_t test1 = {0, 4, tableau1};
  // image_t test2 = {1, 4, tableau2};
  // image_t* image1 = &test1;
  // image_t* image2 = &test2;
  // printf("score image %d \n", scoreImage(image1, image2));
}
