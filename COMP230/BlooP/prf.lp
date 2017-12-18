NB. Primitive Recursive Functions
NB. Procedure for successor
DEFINE PROCEDURE ''S'' [N]:
BLOCK 0: BEGIN
      OUTPUT <= N+1;
BLOCK 0: END.

NB. Procedure for addition
DEFINE PROCEDURE ''PLUS'' [M,N]:
BLOCK 0: BEGIN
      NB. plus(x,0) = 0
      OUTPUT <= 0;
      CELL(0) <= 0;
      OUTPUT <= PLUS [S [M], N-1];
BLOCK 0: END.

NB. Procedure for multiplying
DEFINE PROCEDURE ''MULT'' [M,N]:
BLOCK 0: BEGIN
      NB. mult(x,0) = 0
      OUTPUT <= 0