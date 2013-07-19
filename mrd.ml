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
 * 	SET
 *	CALL
 *	If
 *	Goto
 *	Break
 *	Continue
 *	Return
 *	Block
 *	Switch
 *	Loop
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
 *	Neg (-)
 *	BNot (~) 
 *	LNot (!)
 *	PlusA  	arithmetic +    
 *	PlusPI 	pointer + integer	
 *	IndexPI 	pointer + integer       
 *	MinusA	arithmetic -	
 *	MinusPI	pointer - integer	
 *	MinusPP	pointer - pointer	
 *	Mult
 *	Div	/	
 *	Mod	%	
 *	Shiftlt	shift left	
 *	Shiftrt	shift right	
 *	Lt	< (arithmetic comparison)	
 *	Gt	> (arithmetic comparison)	
 *	Le	<= (arithmetic comparison)	
 *	Ge	> (arithmetic comparison)	
 *	Eq	== (arithmetic comparison)	
 *	Ne	!= (arithmetic comparison)	
 *	BAnd	bitwise and	
 *	BXor	exclusive-or	
 *	BOr	inclusive-or	
 *	LAnd	logical and. 
 *	LOr
 *	SizeOf
 *	CastE
 *	AddrOf
 *	StartOf
 *)
open Cil
open Pretty
open Cfg
open CrestInstrument
module E=Errormsg
(* CONSTANTS *)
let valSet = 11
let valCall = 12
let valIf = 20
let valGoto = 30
let valBreak = 40
let valContinue = 50
let valReturn = 60
let valBlock = 70
let valSwitch = 80
let valLoop = 90

let valNeg = 100
let valBNot = 101
let valLNot = 102
let valPlusA = 200
let valPlusPI = 201    
let valIndexPI = 202
let valMinusA  = 203
let valMinusPI = 204    
let valMinusPP = 205
let valMult = 206
let valDiv = 207  
let valMod = 208 
let valShiftlt = 209   
let valShiftrt = 210
let valLt = 211
let valGt = 212
let valLe = 213
let valGe = 214
let valEq = 215  
let valNe =216
let valBAnd =217
let valBXor =218
let valBOr =219
let valLAnd =220
let valLOr =221
let valSizeOf =300
let valCastE =400
let valAddrOf =500
let valStartOf =600

(* END OF CONSTANTS *)
let buffert = ref ""
let buffere = ref ""
let fcg = ref ""
let cfg = ref ""

let wfcg f1 f2 =
        fcg := (!fcg ^ Printf.sprintf "%d %d\n" f1 f2)

let wcfg i1 i2 =
        cfg := (!cfg ^ Printf.sprintf "%d %d\n" i1 i2)

let wt i t =
	buffert := (!buffert ^ Printf.sprintf "%d %d\n" i t)

let we i e =
	buffere := (!buffere ^ Printf.sprintf "%d %d\n" i e)

let rec mrdvisitExp id (e : exp) =  
   match e with
    SizeOf _ -> we id valSizeOf
  | SizeOfE ex -> mrdvisitExp id ex; we id valSizeOf
  | SizeOfStr _ -> we id valSizeOf
  | UnOp (u,e,_)   -> mrdvisitExp id e;(match u with Neg -> we id valNeg | BNot -> we id valBNot | LNot -> we id valLNot)
  | BinOp (b,e1,e2,_) -> mrdvisitExp id e1;mrdvisitExp id e2;
	(match b with
	 	PlusA -> we id valPlusA
	|	PlusPI -> we id valPlusPI
	|	IndexPI -> we id valIndexPI
	|	MinusA -> we id valMinusA
	|	MinusPI -> we id valMinusPI
	|	MinusPP -> we id valMinusPP
	|	Mult -> we id valMult
	|	Div -> we id valDiv	
	|	Mod -> we id valMod	
	|	Shiftlt -> we id valShiftlt
	|	Shiftrt -> we id valShiftrt
	|	Lt -> we id valLt
	|	Gt -> we id valGt
	|	Le -> we id valLe
	|	Ge -> we id valGe
	|	Eq -> we id valEq
	|	Ne -> we id valNe
	|	BAnd -> we id valBAnd
	|	BXor -> we id valBXor	
	|	BOr -> we id valBOr	
	|	LAnd -> we id valLAnd	
	|	LOr -> we id valLOr	 
)
  | CastE (_,ex)  -> we id valCastE;mrdvisitExp id ex
  | AddrOf _ -> we id valAddrOf
  | StartOf _ -> we id valStartOf
  | AlignOf _ | AlignOfE _ | Const _  | Lval _ -> ()

and mrdvisitInstr id (i : instr) = 
   match i with
    Call(_, e, el,l) -> 
	mrdvisitExp id e; 
	List.iter (mrdvisitExp id) el;
	wt id valCall
  | Set (_,e,l) -> 
	mrdvisitExp id e; 
	wt id valSet
  | Asm _ -> E.s (E.unimp "Asm")

and mrdvisitStmt (s:stmt) = 
   match s.skind with
    Instr il  -> 
	List.iter (mrdvisitInstr s.sid ) il;
  | Return (Some e, l) -> 
	mrdvisitExp s.sid e;
	wt s.sid valReturn
  | Return (None, l)  -> 
	wt s.sid valReturn
  | Goto (p,l) ->
	wt s.sid valGoto
  | Break l ->
	wt s.sid valBreak
  | Continue l ->
	wt s.sid valContinue
  | If (e, blk1, blk2, l) ->
	mrdvisitExp s.sid e;	
	wt s.sid valIf
  | Block b ->
	wt s.sid valBlock
  | Switch(e,blk,sl,l) -> 
	wt s.sid valSwitch
  | Loop _ ->
	wt s.sid valLoop
  | TryExcept _ | TryFinally _ -> 
      E.s (E.unimp "try/except/finally")


(* initialize variables *)
let initFuncProcess (fd : fundec) =
	buffert := ("");
	buffere := ("");
	cfg := ""

let save filename content =
                let f = open_out filename in
                Printf.fprintf f "%s" content ;
                close_out f

(* write result to file *)
let finFuncProcess (fd : fundec) = 
	try
		save ("mrd_st_" ^ fd.svar.vname ) !buffert;
		save ("mrd_et_" ^ fd.svar.vname ) !buffere;
		save ("mrd_cfg_" ^ fd.svar.vname ) !cfg
        with x ->
                failwith ("Failed to write in: " ^ fd.svar.vname ^ "\n")


let rec forallStmts (todo) (fd : fundec) = 
  begin
    fasBlock todo fd.sbody;
  end

and fasBlock (todo) (b : block) =
  List.iter (fasStmt todo) b.bstmts

and fasStmt (todo) (s : stmt) =
  begin
    ignore(todo s);
    match s.skind with
      | Block b -> fasBlock todo b
      | If (_, tb, fb, _) -> (fasBlock todo tb; fasBlock todo fb)
      | Switch (_, b, _, _) -> fasBlock todo b
      | Loop (b, _, _, _) -> fasBlock todo b
      | (Return _ | Break _ | Continue _ | Goto _ | Instr _) -> ()
      | TryExcept _ | TryFinally _ -> E.s (E.unimp "try/except/finally")
  end

let mrdmain (f : file) =
          Simplemem.feature.fd_doit f ;
          iterGlobals f prepareGlobalForCFG ;
          Oneret.feature.fd_doit f ;
          (let ncVisitor = new CrestInstrument.normalizeConditionalsVisitor in
             visitCilFileSameGlobals (ncVisitor :> cilVisitor) f) ;
          clearFileCFG f ;
          readIdCount () ;
          readStmtCount () ;
          readFunCount () ;
          computeFileCFG f ;
          handleCallEdgesAndWriteCfg f; 
	  fcg := "";
	  iterGlobals f (fun g -> match g with GFun (fd,_) -> 
					initFuncProcess fd;
					forallStmts mrdvisitStmt fd;
					finFuncProcess fd;
					()
				| _ -> () );
	  save "fcg" !fcg

let feature : featureDescr =
  { fd_name = "mrd";
    fd_enabled = ref false;
    fd_description = "C File static Analysis for MS Thesis";
    fd_extraopt = [];
    fd_post_check = true;
    fd_doit = mrdmain;
  }
