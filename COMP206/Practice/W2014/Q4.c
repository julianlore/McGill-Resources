int parse (char line[], char buf1[], char buf2[],
	   char buf3[], int bufSize){
  // Line is a comma separated String
  // 3 buffers are empty

  char c = line[0]; // Dummy char for condition
  int fields = 1; // How many fields we have
  // Start at 1 since commas indicate fields+1

  int i= 1; // 1 since we already got first char
  int j = 0;
  int k,l = 0; // 4 indices for 4 Strings
  while(c!='\0'){// Loop until end of String
    if(c==',')fields++; // Comma, inc fields
    else if(fields==1 && j<bufSize){ // 1st buf
      buf1[j]=c;
      j++;
    }
    else if(fields==2 && k<bufSize){ // 2nd
      buf2[k]=c;
      k++;
    }
    else if(fields==3 && l<bufSize){ // 3rd
      buf3[l]=c;
      l++;
    }
    c = line[i];
    i++;
  }
  return fields; // Return number of fields counted
}
