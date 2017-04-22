#include <stdio.h>
#include <stdlib.h>
#include <time.h>
int* genArray(){//Generate a random array
	srand(time(NULL)); // Seed rand using the time
	int* numbers = (int*)malloc(400000); // Allocate 4000 bytes for 1000 ints
	for(int i=0; i<100000; i++){
		numbers[i]=(rand()%10000);
	}
	return numbers;
}

void swap(int* a, int* b){ // To swap two entries
	int temp = *(a); // temp var
	*(a)=*(b); // Assign b to a
	*(b)=temp; // Assign what a was before to b
}

void printArr(int* numbers){ // To print an array of numbers
	for(int i=0; i<100000; i++){
		printf("%d ",*(numbers+i));
	}
}

void bubbleSort(int* numbers){
	for(int i=0; i<99999; i++){
		for(int j=0; j<99999-i; j++){ // Go from 0 to part that isn't sorted yet
			// If prev is bigger than next, swap
			if(*(numbers+j)>*(numbers+j+1)) swap((numbers+j),(numbers+j+1));
		}
	}
}

int main(int argc, char* argv[]){
	int* numbers = genArray(); // Generate numbers
	printf("Before sort:\n");
	printArr(numbers);
	bubbleSort(numbers);
	printf("\n After sort:\n");
	printArr(numbers);
	printf("\n");
	free(numbers); // Free malloced memory
	return 0; // Success exit code
}
