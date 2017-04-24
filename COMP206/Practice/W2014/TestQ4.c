#include<stdio.h>
#include"Q4.c"

int main(int argc, char* argv[]){
  // argv[1] is the string you want to test on
  char b[10],c[10],d[10];
  int n = 10;
  int x = parse(argv[1], b, c, d, n);
  printf("line:%s \n buf1:%s \n buf2: %s \n buf3: %s\n count: %d\n",argv[1],b,c,d,x);
}
