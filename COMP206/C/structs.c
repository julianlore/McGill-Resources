#include <stdio.h>
#include<stdlib.h>
typedef struct STUDENT{
	char name[30];
	int age;
	double gpa;
}test;

enum days{mon,tue};

int main(int argc, char* argv[]){

union NUMBER{
	short int a;
	int b;
	float c;
	double d;
}number;
	union NUMBER a;
	a.a=5;	
	test z;
	z.name[0]='J';
	z.name[1]='\0';
	struct STUDENT *q;
	q=(struct STUDENT*)malloc(sizeof(struct STUDENT));
	q->age=20;
	//printf("%s",z.name);
	int x = mon + tue;
	printf("%d",a.b);
}
