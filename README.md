In The Name of Allah

cilExt
======
CIL Extention for Whitebox Fuzzer


Reads a C File and produce following files
 1. Program 'Function Call Graph' or FCG
 2. Intra function 'Flow Control Graph' or CFG for each function
 3. Statement type Table foreach function
 4. Experisson type Table foreach function
 
Statement type Table:
     -------------------------------------
     |  Statement Id |    Statement Type |
     -------------------------------------
     |               |                   |
   
   i. Statement Id is CIL s.sid
   ii. Statement Type:
  1 for Instruction set
	2 for If
	3 for Goto
	4 for Break
	5 for Continue
	6 for Return
	7 for Block
	8 for Switch
	9 for Loop

Expression type Table:
     If the statement recorded in previous table has a expression part, we 
   record it's expression types in this table
     ---------------------------------------
     |  Statement Id |    Experission Type |
     ---------------------------------------
     |               |                     |

   i. Foreign Key to Previous Table
   ii. Expression Type:
	100 Unary operator - Neg (-)
	101 Unary operator - BNot (~) 
	102 Unary operator - LNot (!)
	200 Binary operator - PlusA  (*	arithmetic +    *)
	201 Binary operator - PlusPI (*	pointer + integer	*)
	202 Binary operator - IndexPI (*	pointer + integer       *)
	203 Binary operator - MinusA	(*	arithmetic -	*)
	204 Binary operator - MinusPI	(*	pointer - integer	*)
	205 Binary operator - MinusPP	(*	pointer - pointer	*)
	206 Binary operator - Mult
	207 Binary operator - Div	(*	/	*)
	208 Binary operator - Mod	(*	%	*)
	209 Binary operator - Shiftlt	(*	shift left	*)
	210 Binary operator - Shiftrt	(*	shift right	*)
	211 Binary operator - Lt	(*	< (arithmetic comparison)	*)
	212 Binary operator - Gt	(*	> (arithmetic comparison)	*)
	213 Binary operator - Le	(*	<= (arithmetic comparison)	*)
	214 Binary operator - Ge	(*	> (arithmetic comparison)	*)
	215 Binary operator - Eq	(*	== (arithmetic comparison)	*)
	216 Binary operator - Ne	(*	!= (arithmetic comparison)	*)
	217 Binary operator - BAnd	(*	bitwise and	*)
	218 Binary operator - BXor	(*	exclusive-or	*)
	219 Binary operator - BOr	(*	inclusive-or	*)
	220 Binary operator - LAnd	(*	logical and. *)
	221 Binary operator - LOr
	300 SizeOf
	400 CastE
	500 AddrOf
	600 StartOf

