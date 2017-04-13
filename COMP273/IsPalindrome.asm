	.data
STR1:	.asciiz "Hello world"
STR2:	.asciiz "anutforajaroftuna"
	
	.text
	.globl main
main:		la $t0, STR2
		li $t1, 0
		li $t2, 16
		
		subi $sp, $sp, 12
		sw $t0, 8($sp)
		sw $t1, 4($sp)
		sw $t2, 0($sp)
		jal IsPalindrome
		j Exit
		
IsPalindrome:	lw $t0, 8($sp)	# First arg, String pointer
		lw $t1, 4($sp)	# 2nd arg, starting index
		lw $t2, 0($sp)	# 3rd arg, end index
		
		add $t3,$t0,$t1	# Get address of first char
		add $t4,$t0,$t2	# Get address of 2nd char
		
		lb $t5,($t3)	# Get starting char
		lb $t6,($t4)	# Get last char
		
		beq $t5,$t6,Rec	# Recursive if same
		
		# Else
		li $v0, 0	# False
		addi $sp,$sp,12	# Pop everything
		jr $ra		# Go back
		
Rec:	sub $t7,$t4,$t3		# Difference in indexes of chars
	li $v0, 1		# Set to true for now
	sle $t8, $t7, 1		# If index is same or 1 apart
	bne $t8,$zero,Return	# If not false, go back
	addi $t1, $t1, 1	# Else increment and decrement char indices
	subi $t2, $t2, 1
	sw $t1, 4($sp)		# Pass arguments back on the stack
	sw $t2, ($sp)
	j IsPalindrome		# Jump to recursive call

Return:	jr $ra

Exit: