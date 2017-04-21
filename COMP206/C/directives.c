#include <stdio.h>
#define MAX(A,B) (A<B)?B:A
#ifdef TEST
#define TEST2 10
#endif
#ifndef TEST
void test(){
	printf("Here it is\n");
}
#endif

int main(int arcg, char* argv[]){
	int max = MAX(5,10);
	test();
}
