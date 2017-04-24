main: main.o library.o menu.o order.o report.o
# All .o files must exist
# Compile under main
	gcc -o main main.o main.o library.o menu.o order.o report.o

main.o: main.c main.h library.h menu.h order.h report.h
	gcc -c main.c

order.o: order.c order.h
	gcc -c order.c

report.o: report.c report.h
	gcc -c report.c

library.o: library.c library.h
	gcc -c library.c

menu.o: menu.c menu.h
	gcc -c menu.c

backup:
	if [!-d backup] then #If backup doesn't exist
		mkdir backup # Create the new dir
	fi
# copy all .c and .h files into backup
	cp *.c *.h backup

tar:
	if [!-d backup] then # Backup doesn't exist
		echo"Backup directory does not exist,
			nothing was TAR'ed"
	else
	cd backup # Go to backup dir
	tar -cf * backup.tar # Tar all files
	rm *.c *.h # Remove all .c and .h files
