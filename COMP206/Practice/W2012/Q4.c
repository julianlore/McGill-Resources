#include <stdio.h>
#include <string.h>

void main(){
  int n = atoi(getenv("CONTENT_LENGTH"));
  char in[n+1];
  fgets(in, n+1, stdin); // Get the input from POST
  // Read csv
  FILE* in = fopen("web.csv","rt");
  char desc[100];
  char site[50];
  while(fscanf(in,"%s,%s",site,desc)!=EOF){
    // How many words are there in desc
    int words = 1;
    for(int i=0; i<strlen(desc); i++){
      if(fgetc(desc)=" "){
	words++;
      }
    }
    // Tokenize 
  }
}
