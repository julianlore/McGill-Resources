#include <stdio.h>
#include <stdlib.h>
#include <string.h>
struct STUDENT{
  char name[50];
  double gpa;
};

int main(int argc, char* argv[]){
  int n; // Input of user
  printf("How many students in the classroom?\n");
  scanf("%d",&n); // Scan number into n
  struct STUDENT *p; // Pointer for array of students
  // Malloc for n students
  p = (struct STUDENT *)malloc(n*(sizeof(struct STUDENT)));
  int j = 0; // Counter for how many entries csv has

  FILE* in = fopen("GPA.CSV","rt"); // Open file
  if(in==NULL){ //File doesn't exist
    printf("GPA.CSV does not exist! \n");
  }
  else{
    // For storing values
    char name[50];
    double gpa;
    // Scan until EOF
    while(scanf("%s,%lf",name,&gpa)!=EOF){
      j++; // Increment amount of entries
      if(j<n){ // Did not hit max yet
	// p+j-1 since j is one bigger than index
	strcpy((p+j-1)->name,name); // copy name
	(p+j-1)->gpa = gpa; // Copy gpa
      }
    }
    fclose(in); // Close input
  }
  // Loop to get average, j entries
  double average = 0;
  for(int i=0; i<j; i++){
    average += (p+i)->gpa; // Sum gpas
  }
  average /= j; // Divide by number of entries
  printf("Average GPA: %lf",average);
}
