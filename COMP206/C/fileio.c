#include <stdio.h>
int main(int argc, char* argv[]){
	FILE* out = fopen("test.txt", "wt"); // wt for writing text
	fprintf(out, "Hello World\n"); // Write Hello World to file
	fclose(out); // Close the file opened, end with EOF char

	FILE* in = fopen("test.txt", "rt"); // rt for reading text
	int numbers; // Int for number of chars in file
	while(fgetc(in)!=EOF){ // Get char until EOF
		numbers++; // Keep incrementing as long as characters are valid
	}
	char file[numbers]; // Char array for numbers amount of chars
	rewind(in); // Rewind to the beginning of the file
	for(int i=0; i<numbers; i++){ // Loop through file
		file[i]=fgetc(in); // Get each char one by one
	}
	fclose(in); // Done with file now
	printf("%s", file); // Print the resulting String loaded
}
