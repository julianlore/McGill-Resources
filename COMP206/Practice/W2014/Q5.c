#include <stdio.h>
#include <ctype.h>
#include <string.h>
// Struct with 3 fields requested
struct STUDENT{
  char name[50];
  int age;
  double gpa;
};

int main(int argc, char* argv[]){
  // Prompt user
  printf("Enter the size of the array: \n");
  int n; // To store input
  int i; // Counter for students
  char garbage; // To collect CR
  scanf("%d",&n); // Scan input into n
  scanf("%c",&garbage); // Collect CR
  struct STUDENT data[n]; // Array of n students
  int o = 0; // Use o to ask for options
  while(o!=4){//Loop until (4)Quit
    printf("(1) Add a student \n"
	   "(2) Delete a student\n"
	   "(3) Save all students\n"
	   "(4) Quit\n");
    scanf("%d",&o);
    scanf("%c",&garbage); // Collect CR
    if(o == 1){ // Add a student
      printf("Name: \n");
      char name[50]; 
      scanf("%s",name);
      scanf("%c",&garbage); // Collect CR
      printf("Age: \n");
      int age;
      scanf("%d",&age);
      scanf("%c",&garbage); // Collect CR
      printf("GPA: \n");
      double gpa;
      scanf("%lf",&gpa);
      scanf("%c",&garbage); // Collect CR
      int t = n; // Temp var for condition
      while(t>=n||t<0){ // While index is invalid
	printf("What cell should they be placed in?\n");
	scanf("%d",&t);
	scanf("%c",&garbage); // Collect CR
	if(t>=n||t<0){
	  printf("Invalid cell number! \n");
	}
      }
      // Assign all data
      strcpy(data[t].name,name);
      data[t].age = age;
      data[t].gpa = gpa;
    }
    else if(o == 2){// Delete
      // Could have used a function here
      int t = n;
      while(t>=n||t<0){
	printf("What cell do you want to delete?\n");
	scanf("%d",&t);
	scanf("%c",&garbage); // Collect CR
	if(t>=n||t<0){
	  printf("Invalid cell number! \n");
	}
      }
      for(int i=0; i<50; i++){ // Set name to null
	data[t].name[i] = '\0';
      }
      data[t].age = 0;
      data[t].gpa = 0;
    }
    else if(o == 3){// Save
      int save; // Save or not?
      // Check if file exists
      FILE* in = fopen("students.csv","rt");
      if(in!=NULL){
	printf("File exists already, overwrite?(y/n)\n");
	char ans;
	scanf("%c",&ans);
	scanf("%c",&garbage); // Collect CR
	if(ans == 'y'){
	  save = 1; // True to save
	}
	else{
	  save = 0; // Don't save
	}
      fclose(in); // Close the reader
      }
      else{
	save = 1; // Overwrite if nothing found
      }
      if(save == 1){ // If we want to save
	FILE* out = fopen("students.csv","wt");
	for(int i=0; i<n; i++){ // Loop through all cells
	  if (isalpha(data[i].name[0])){ // Name starts with letter
	    fprintf(out,"%s,%d,%1.1lf \n",data[i].name, data[i].age,
		    data[i].gpa); // Write fields
	  }
	}
	fclose(out); // Close out
      }
      
    }
  }
}
