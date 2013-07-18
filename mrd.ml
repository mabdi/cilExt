(*  In The Name of Allah
 *  
 *
 * Reads a C File and produce following files
 *	1. Program 'Function Call Graph' or FCG
 *	2. Intra function 'Flow Control Graph' or CFG for each function
 *	3. Statement type Table foreach function
 *	4. Experisson type Table foreach function
 *
 *
 * Statement type Table:
 *      -------------------------------------
 *	|  Statement Id |    Statement Type |
 *      -------------------------------------
 *      |               |                   |
 *    i. Statement Id is CIL s.sid
 *    ii. Statement Type:
 * 	1 for Instruction set
 *	2 for If
 *	3 for Goto
 *	4 for Break
 *	5 for Continue
 *	6 for Return
 *	7 for Block
 *	8 for Switch
 *	9 for Loop
 *
 *
 * Expression type Table:
 *      If the statement recorded in previous table has a expression part, we 
 *    record it's expression types in this table
 *      ---------------------------------------
 *	|  Statement Id |    Experission Type |
 *      ---------------------------------------
 *      |               |                     |
 *
 *    i. Foreign Key to Previous Table
 *    ii. Expression Type:
 *	100 Unary operator - Neg (-)
 *	101 Unary operator - BNot (~) 
 *	102 Unary operator - LNot (!)
 *	200 Binary operator - PlusA  (*	arithmetic +    *)
 *	201 Binary operator - PlusPI (*	pointer + integer	*)
 *	202 Binary operator - IndexPI (*	pointer + integer       *)
 *	203 Binary operator - MinusA	(*	arithmetic -	*)
 *	204 Binary operator - MinusPI	(*	pointer - integer	*)
 *	205 Binary operator - MinusPP	(*	pointer - pointer	*)
 *	206 Binary operator - Mult
 *	207 Binary operator - Div	(*	/	*)
 *	208 Binary operator - Mod	(*	%	*)
 *	209 Binary operator - Shiftlt	(*	shift left	*)
 *	210 Binary operator - Shiftrt	(*	shift right	*)
 *	211 Binary operator - Lt	(*	< (arithmetic comparison)	*)
 *	212 Binary operator - Gt	(*	> (arithmetic comparison)	*)
 *	213 Binary operator - Le	(*	<= (arithmetic comparison)	*)
 *	214 Binary operator - Ge	(*	> (arithmetic comparison)	*)
 *	215 Binary operator - Eq	(*	== (arithmetic comparison)	*)
 *	216 Binary operator - Ne	(*	!= (arithmetic comparison)	*)
 *	217 Binary operator - BAnd	(*	bitwise and	*)
 *	218 Binary operator - BXor	(*	exclusive-or	*)
 *	219 Binary operator - BOr	(*	inclusive-or	*)
 *	220 Binary operator - LAnd	(*	logical and. *)
 *	221 Binary operator - LOr
 *	300 SizeOf
 *	400 CastE
 *	500 AddrOf
 *	600 StartOf
 *)
open Cil
open Pretty
module E=Errormsg

let buffert = ref ""
let buffere = ref ""

let wt i t =
	buffert := (!buffert ^ Printf.sprintf "%d %d\n" i t)

let we i e =
	buffere := (!buffere ^ Printf.sprintf "%d %d\n" i e)

let rec mrdvisitExp id (e : exp) =  
   match e with
    SizeOf -> we id 300
  | SizeOfE e -> mrdvisitExp id e;we id 300
  | SizeOfStr -> we id 300
  | UnOp (u,e,_)   -> mrdvisitExp id e;(match u with Neg -> we id 100 | BNot -> we id 101 | LNot -> we id 102)
  | BinOp (b,e1,e2,_) -> mrdvisitExp e1;mrdvisitExp e2;
	(match b with
	 	PlusA -> we id 200
	|	PlusPI -> we id 201
	|	IndexPI -> we id 202	
	|	MinusA -> we id 203	
	|	MinusPI -> we id 204	
	|	MinusPP -> we id 205	
	|	Mult -> we id 206
	|	Div -> we id 207	
	|	Mod -> we id 208	
	|	Shiftlt -> we id 209
	|	Shiftrt -> we id 210	
	|	Lt -> we id 211	
	|	Gt -> we id 212	
	|	Le -> we id 213	
	|	Ge -> we id 214	
	|	Eq -> we id 215	
	|	Ne -> we id 216	
	|	BAnd -> we id 217
	|	BXor -> we id 218	
	|	BOr -> we id 219	
	|	LAnd -> we id 220	
	|	LOr -> we id 221	 
)
  | CastE (_,e)  -> we s.sid 400;mrdvisitExp e
  | AddrOf  -> we s.sid 500
  | StartOf -> we s.sid 600
  | AlignOf  | AlignOfE  | Const  | Lval -> ()

and mrdvisitInstr id (i : instr) = 
   match i with
    Call(_, e, el,l) -> 
	mrdvisitExp id e; 
	List.iter (mrdvisitExp id) el
  | Set (_,e,l) -> 
	mrdvisitExp id e; 
  | Asm -> E.s (E.unimp "Asm")

and mrdvisitStmt (s : stmt) = 
   match s.skind with
    Instr il  -> 
	List.iter (mrdvisitInstr s.sid ) il;
	wt s.sid 1
  | Return (Some e, l) -> 
	mrdvisitExp id e;
	wt s.sid 6
  | Return (None, l)  -> 
	wt s.sid 6
  | Goto (p,l) -> !p
	wt s.sid 3
  | Break l -> ()
	wt s.sid 4
  | Continue l -> ()
	wt s.sid 5
  | If (e, blk1, blk2, l) ->
	mrdvisitExp id e;	
	wt s.sid 2
  | Block b ->
	wt s.sid 7 
  | Switch(e,blk,sl,l) -> 
	wt s.sid 8
  | Loop(blk,l,Some e,None) ->
	wt s.sid 9
  | Loop(blk,l,None,Some e) ->
	wt s.sid 9
  | Loop(blk,l,None,None) ->
	wt s.sid 9
  | TryExcept _ | TryFinally _ -> 
      E.s (E.unimp "try/except/finally")


(* initialize variables *)
let initFuncProcess (fd : fundec) =


(* write result to file *)
let finFuncProcess (fd : fundec) = 

let main f:file =
          Simplemem.feature.fd_doit f ;
          iterGlobals f prepareGlobalForCFG ;
          Oneret.feature.fd_doit f ;
          (let ncVisitor = new CrestInstrument.normalizeConditionalsVisitor in
             visitCilFileSameGlobals (ncVisitor :> cilVisitor) f) ;
          Cfg.clearFileCFG f ;
          readIdCount () ;
          readStmtCount () ;
          readFunCount () ;
          Cfg.computeFileCFG f ;
          handleCallEdgesAndWriteCfg f ;
	  iterGlobals f (fun g ->
    		match g with GFun(fd,_) -> begin
				initFuncProcess fd;
				Cfg.forallStmts mrdvisitInst fd;
				finFuncProcess fd;
			end
    		| _ -> ()
	  )


let feature : featureDescr =
  { fd_name = "mrd";
    fd_enabled = ref false;
    fd_description = "C File static Analysis for MS Thesis";
    fd_extraopt = [];
    fd_post_check = true;
    fd_doit = main
  }
