(* ******************************************************************************
Class:          TMathParser
Version:        1.1
Date:           2004-03-24

Author:         Michael Elsdörfer
Copyright:      (c)2002/2003 by Michael Eldsörfer
eMail:          michael@elsdoerfer.net
Internet:       http:// www.elsdoerfer.net

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at 
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Michael Elsdörfer.
All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
****************************************************************************** *)

unit MathParser;

{$mode delphi}

interface

uses Classes, SysUtils, Math;

const
  (* operator priority constants *)
  MPOP_OPERATOR_ADDITION        = 0;       // used for '+' operator
  MPOP_OPERATOR_SUBTRACTION     = 1;       // used for '-' operator
  MPOP_OPERATOR_MULTIPLICATION  = 2;       // used for '*' operator
  MPOP_OPERATOR_DIVISION        = 2;       // used for '/' and 'div' operator
  MPOP_OPERATOR_LEFTRIGHT       = 3;       // use for operators which use both left and right arguments
  MPOP_OPERATOR_RIGHTONLY       = 4;       // use for operators which use only right arguments
  MPOP_OPERATOR_LEFTONLY        = 5;       // use for operators which use only left arguments
  MPOP_OPERATOR_NONE            = 6;       // use for operators which use no arguments

type
 (* error codes *)
 TMPError = (mpeNone,                      // everything is ok, no error
             mpeUnknownIdentifier,         // identifier is not a valid operator or constant
             mpeInvalidArgument,           // the argument is not valid
             mpeMissingArgument,           // a argument is missing
             mpeUnexpectedArgument,        // the operator does not allow the argument
             mpeBracketNotOpened,          // the bracket was closed, but not opened
             mpeClosingBracketExpected,    // there are unclosed brackets
             mpeDivisionByZero,            // division by zero
             mpeCustom);                   // custom error for user operators

 (* operator declarations *)
 TMPArgument = (mpaLeft, mpaRight);
 TMPArguments = set of TMPArgument;

 TMPOperatorFunction = function(
                         LeftArg, RightArg: Extended;        // left and right arguments (zero if none)
                         AssignedArguments: TMPArguments;    // all arguments used
                         var Error: TMPError;                // by operator function speficied error
                         var ErrorDescription: string;       // custom error description
                         var InvalidArguments: TMPArguments) // arguments which caused the error (if any)
                       : Extended;

 PTMPOperator = ^TMPOperator;
 TMPOperator = record
   OpName: string;                     // operator name (case-intensitive, no spaces or brackets)
   OpArguments: TMPArguments;          // allowed arguments
   OpFunction: TMPOperatorFunction;    // operator function
   OpPriority: ShortInt;               // operator priority (see constants)
 end;

 (* constant declaraion *)
 PTMPConstant = ^TMPConstant;
 TMPConstant = record
   CName: string;
   CValue: Extended;
 end;

 (* term tree declarations *)
 TTermTreeNodeType = (ttntValue, ttntOperator);

 PTTermTreeNode = ^TTermTreeNode;
 TTermTreeNode = record
   NodeType: TTermTreeNodeType;               // used to differ value and operators nodes
   LeftNode, RightNode: PTTermTreeNode;       // pointer to left and right subnode
   Operator: PTMPOperator;                    // only for operator nodes
   Value: Extended;                           // only for value nodes
   ValueConstant: PTMPConstant;               // only for value nodes  
   ValueAssigned: boolean;                    // only for value nodes
   OperatorPos, LeftArgPos, LeftArgLength,    // only for operator nodes
     RightArgPos, RightArgLength: integer;
 end;

 (* TMathParser Exception *)
 EMathParserException = class(Exception)
 public
   Error: TMPError;
   ErrorPos: integer;
   ErrorLength: integer;
   ErrorDescription: string;

   constructor Create(FError: TMPError; FErrorPos,
      FErrorLength: integer; FErrorDescription: string);
 end;

 (* TMathParser Class *)
 TMathParser = class(TObject)
 private
    FOperatorList: TList;
    FConstantList: TList;

    FParseError: TMPError;
    FParseErrorPos: integer;
    FParseErrorLength: integer;
    FParseErrorDescription: string;
    FLastTermTree: PTTermTreeNode;

    function GetOperators(Name: string): TMPOperator;
    procedure SetOperators(Name: string; const Value: TMPOperator);
    procedure SetParseErrorPos(const Value: integer);
    procedure SetParseErrorLength(const Value: integer);
    procedure SetParseErrorDescription(const Value: string);
    procedure SetParseError(const Value: TMPError);
    function GetConstants(Name: string): TMPConstant;
    procedure SetConstants(Name: string; const Value: TMPConstant);
    function GetConstantValues(const Name: String): Extended;
    procedure SetConstantValues(const Name: String; const Value: Extended);
 protected
    function DoParseTerm(ATerm: string): PTTermTreeNode;
    function GetOperatorListIndex(Name: string): integer;
    function GetConstantListIndex(Name: string): integer;
 public
    constructor Create;
    destructor Destroy; override;

    procedure AddOperator(Name: String; OperatorFunction: TMPOperatorFunction;
      Arguments: TMPArguments);
    procedure AddOperatorEx(Name: String;
       OperatorFunction: TMPOperatorFunction; Arguments: TMPArguments;
       Priority: SmallInt);
    procedure DeleteOperator(Name: String);
    function OperatorCount: integer;

    procedure AddConstant(Name: string; Value: Extended);
    procedure DeleteConstant(Name: string);
    function ConstantCount: integer;

    function ParseTerm(ATerm: string): PTTermTreeNode;
    function CalcTree(ATreeRootNode: PTTermTreeNode): Extended;

    function FormatTerm(ATermTree: PTTermTreeNode): string;

    procedure FreeTermTree(ATermTree: PTTermTreeNode);
    procedure FreeLastTermTree;

    procedure RegisterStandardOperators;
    procedure RegisterExtendedOperators;
    procedure RegisterConversionOperators;

    property Operators[Name: string]: TMPOperator read GetOperators write SetOperators;
    property Constants[Name: string]: TMPConstant read GetConstants write SetConstants;
    property ConstantValues[const Name: String]: Extended read GetConstantValues write SetConstantValues;
    
    property ParseError: TMPError read FParseError write SetParseError;
    property ParseErrorDescription: string read FParseErrorDescription write SetParseErrorDescription;
    property ParseErrorPos: integer read FParseErrorPos write SetParseErrorPos;
    property ParseErrorLength: integer read FParseErrorLength write SetParseErrorLength;    
 end;

(* MathParser trim functions: return number of trimed chars *) 
function MPTrim(var AString: string): integer;
function MPTrimCount(AString: string): integer;

(* standard operator functions *)

function MPOperatorAddition(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorSubtraction(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorMultiplication(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
  
function MPOperatorDivision(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorSin(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorCos(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorTan(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorPower(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorSqrt(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorLn(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorLog(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorArcSin(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorArcCos(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorArcTan(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorSinH(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorCosH(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorTanH(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorMod(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorDiv(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorAbs(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorFloor(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorCeil(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorExp(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorExp2(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;


(* extended operator functions *)

function MPOperatorMax(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorMin(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorArcCosh(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorArcSinh(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorArcTanh(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorArcTan2(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorCot(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorCotan(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorHypot(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorSec(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorRandom(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorNSqrt(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

(* conversion operator functions *)

function MPOperatorDeg2Rad(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorDeg2Grad(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorRad2Deg(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorRad2Cycle(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorRad2Grad(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorGrad2Rad(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

function MPOperatorGrad2Deg(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;

  
implementation

{ TMathParser }

procedure TMathParser.AddConstant(Name: string; Value: Extended);
var
  NewConstant: PTMPConstant;
begin
  New(NewConstant);
  with NewConstant^ do Begin
    CName:=LowerCase(Name);
    CValue:=Value;
  end;
  FConstantList.Add(NewConstant);
end;

procedure TMathParser.AddOperator(Name: String;
  OperatorFunction: TMPOperatorFunction; Arguments: TMPArguments);
begin
  if Arguments=[mpaLeft,mpaRight] then
    AddOperatorEx(Name,OperatorFunction,Arguments,MPOP_OPERATOR_LEFTRIGHT)
  else if Arguments=[mpaLeft] then
    AddOperatorEx(Name,OperatorFunction,Arguments,MPOP_OPERATOR_LEFTONLY)
  else if Arguments=[mpaRight] then
    AddOperatorEx(Name,OperatorFunction,Arguments,MPOP_OPERATOR_RIGHTONLY)
  else AddOperatorEx(Name,OperatorFunction,Arguments,MPOP_OPERATOR_NONE);
end;

procedure TMathParser.AddOperatorEx(Name: String;
  OperatorFunction: TMPOperatorFunction; Arguments: TMPArguments;
  Priority: SmallInt);
var NewOperator: PTMPOperator;
begin
  New(NewOperator);
  with NewOperator^ do Begin
    OpName:=LowerCase(Name);
    OpArguments:=Arguments;
    OpFunction:=OperatorFunction;
    OpPriority:=Priority;
  end;
  FOperatorList.Add(NewOperator);
end;

function TMathParser.CalcTree(ATreeRootNode: PTTermTreeNode): Extended;
var AError: TMPError;

    function DoCalcTree(ATreeRootNode: PTTermTreeNode; var Error: TMPError): Extended;
    var OpArguments, OpInvalidArgs: TMPArguments;
        OpError: TMPError;
        OpErrorDesc: string;
        LeftRes, RightRes: Extended;
    Begin
       // if there was an error, break
       if Error<>mpeNone then exit;
       // case node type
       result:=-1;
       case ATreeRootNode^.NodeType of
         ttntOperator: Begin
           // prepar call of operator function 
           OpArguments:=[];
           OpInvalidArgs:=[];
           OpError:=mpeNone;
           if ATreeRootNode^.LeftNode.ValueAssigned then OpArguments:=OpArguments+[mpaLeft];
           if ATreeRootNode^.RightNode.ValueAssigned then OpArguments:=OpArguments+[mpaRight];
           // call operator function
           LeftRes:=DoCalcTree(ATreeRootNode^.LeftNode,Error);
           RightRes:=DoCalcTree(ATreeRootNode^.RightNode,Error);
           if Error<>mpeNone then exit;

           try
             result:=ATreeRootNode^.Operator^.OpFunction(LeftRes,RightRes,OpArguments,OpError,OpErrorDesc,OpInvalidArgs);
           except
             on E: Exception do Begin
               Error:=mpeCustom;
               ParseError:=mpeCustom;
               ParseErrorPos:=ATreeRootNode^.OperatorPos;
               ParseErrorLength:=length(ATreeRootNode^.Operator^.OpName);
               ParseErrorDescription:=E.Message;
               raise EMathParserException.Create(ParseError,ParseErrorPos,ParseErrorLength,ParseErrorDescription);
             end;
           end;
           // test, if there is an error
           if OpError<>mpeNone then Begin
             Error:=OpError;
             ParseError:=OpError;                        
             ParseErrorDescription:=OpErrorDesc;
             // assign error pos
             if (mpaRight in OpInvalidArgs) then Begin           // right argument is incorrect
               ParseErrorPos:=ATreeRootNode^.RightArgPos;
               ParseErrorLength:=ATreeRootNode^.RightArgLength;
             end else if (mpaLeft in OpInvalidArgs) then Begin   // left argument is incorrect
               ParseErrorPos:=ATreeRootNode^.LeftArgPos;
               ParseErrorLength:=ATreeRootNode^.LeftArgLength;
             end else Begin                                      // not speficied - use operator pos
               ParseErrorPos:=ATreeRootNode^.OperatorPos;
               ParseErrorLength:=length(ATreeRootNode^.Operator^.OpName);
             end;

             raise EMathParserException.Create(ParseError,ParseErrorPos,ParseErrorLength,ParseErrorDescription);
           end;
         end;
         
         ttntValue: Begin
           if ATreeRootNode.ValueConstant=nil then result:=ATreeRootNode.Value
           else result:=ATreeRootNode.ValueConstant.CValue;
         end;
       end;
    end;


begin
  if ATreeRootNode=nil then result:=0
  else Begin
    AError:=mpeNone;
    result:=DoCalcTree(ATreeRootNode, AError);
    if AError<>mpeNone then result:=0;
  end;
end;

function TMathParser.ConstantCount: integer;
begin
 result:=FConstantList.Count;
end;

constructor TMathParser.Create;
begin
  FOperatorList:=TList.Create;
  FConstantList:=TList.Create;

  ParseError:=mpeNone;
  ParseErrorPos:=-1;
  ParseErrorLength:=0;

  RegisterStandardOperators;
end;

procedure TMathParser.DeleteConstant(Name: string);
var APos: integer;
begin
  APos:=GetConstantListIndex(Name);
  if APos<>-1 then Begin
    Dispose(FConstantList[APos]);
    FConstantList.Delete(APos);
  end else raise Exception.Create('There is no constant named "'+Name+'".');
end;

procedure TMathParser.DeleteOperator(Name: String);
var APos: integer;
begin
  APos:=GetOperatorListIndex(Name);
  if APos<>-1 then Begin
    Dispose(FOperatorList[APos]);
    FOperatorList.Delete(APos);
  end else raise Exception.Create('There is no operator named "'+Name+'".');
end;

destructor TMathParser.Destroy;
var i: integer;
begin
  // dispose operators
  for i:=0 to FOperatorList.Count-1 do Dispose(FOperatorList[i]);
  // free operator list
  FOperatorList.Free;

  // dispose constants
  for i:=0 to FConstantList.Count-1 do Dispose(FConstantList[i]);
  // free constant list
  FConstantList.Free;
 
  inherited;
end;

function TMathParser.DoParseTerm(ATerm: string): PTTermTreeNode;
var ARootNode: PTTermTreeNode;

    function IsNotNumeric(AChar: Char): boolean;
    Begin
      if pos(AChar,'0123456789 .,')>0 then result:=False
      else result:=True;
    end;

    function GetNextOperatorPos(ATerm: string; var StartPos, EndPos: integer;
      SearchFrom: integer): boolean;
    var i: integer;
        OpenBrackets: integer;
        FPos: integer;
    Begin
      StartPos:=-1;
      EndPos:=-1;
      OpenBrackets:=0;
      FPos:=-1;
      result:=false;
      
      for i:=SearchFrom downto 1 do Begin
        // check if found part is already an operator or constant
        if (EndPos<>-1) and
           (
             (GetOperatorListIndex(copy(ATerm,i+1,EndPos-i))>-1) or
             (GetConstantListIndex(copy(ATerm,i+1,EndPos-i))>-1)
           ) then FPos:=i+1;

        if (ATerm[i]=')') then Begin
          if EndPos<>-1 then Begin
            StartPos:=i+1;
            break;
          end else inc(OpenBrackets);
        end else if (ATerm[i]='(') then dec(OpenBrackets)
        else if (IsNotNumeric(ATerm[i])) and (OpenBrackets=0) then Begin
           if (EndPos=-1) then EndPos:=i;
        end
        else if EndPos<>-1 then
        begin
          StartPos:=i+1;
          break;
        end;
      end; // end for

      if (StartPos=-1) and (EndPos<>-1) then StartPos:=1;      

      // if the found identifier is not valid, but there is a valid part identifier, use this
      if ( (StartPos=-1) or
           ((GetOperatorListIndex(copy(ATerm,StartPos,EndPos-StartPos+1))<=-1) and
           (GetConstantListIndex(copy(ATerm,StartPos,EndPos-StartPos+1))<=-1))
         ) and
         (FPos<>-1) and
         (
           (GetOperatorListIndex(copy(ATerm,FPos,EndPos-FPos+1))>-1) or
           (GetConstantListIndex(copy(ATerm,FPos,EndPos-FPos+1))>-1)
         ) then StartPos:=FPos;

      result:=StartPos<>-1;
    end;

    function TermToTree(ATerm: String; AParentNode: PTTermTreeNode; PosInTerm: integer): boolean;
    var ALeft, ARight, OpName: string;
        NewNodeL, newNodeR: PTTermTreeNode;
        StartPos, EndPos, OpIndex, OpPos, oi, sp, ep, LowestPriority, t: integer;
        TrimCount: integer;

        function GetArgumentPos(Arg: TMPArgument): integer;
        Begin
          case Arg of
            mpaLeft:  result:=TrimCount+PosInTerm;
            mpaRight: result:=PosInTerm+StartPos+(EndPos-StartPos)+TrimCount;
          end;
        end;

    Begin
      result:=true;

      // trim
      TrimCount:=MPTrim(ATerm);

      // cut brackets
      while (not GetNextOperatorPos(ATerm,sp,ep,length(ATerm))) and
            (ATerm<>'') and (ATerm[1]='(') and (ATerm[length(ATerm)]=')') do Begin
        ATerm:=copy(ATerm,2,length(ATerm)-2);
        TrimCount:=TrimCount+MPTrim(ATerm)+1;
      end;

      // find operator with lowest priority
      OpIndex:=-1; 
      OpPos:=length(ATerm);
      LowestPriority:=High(SmallInt);
      StartPos:=-1; EndPos:=-1;
      repeat
        if not GetNextOperatorPos(ATerm,sp,ep,OpPos) then Begin
          break;
        end;
        if sp>-1 then Begin
          OpName:=copy(ATerm,sp,ep-sp+1);
          if GetConstantListIndex(OpName)<=-1 then Begin
            oi:=GetOperatorListIndex(OpName);
            if oi=-1 then Begin
              result:=false;
              ParseError:=mpeUnknownIdentifier;
              ParseErrorPos:=PosInTerm+sp+TrimCount-1;
              ParseErrorLength:=ep-sp+1;
              raise EMathParserException.Create(ParseError,ParseErrorPos,ParseErrorLength,ParseErrorDescription);
            end else Begin
              if PTMPOperator(FOperatorList[oi]).OpPriority < LowestPriority then Begin
                StartPos:=sp;
                EndPos:=ep;
                LowestPriority:=PTMPOperator(FOperatorList[oi]).OpPriority;
                OpIndex:=oi;
              end;
              OpPos:=sp-1;
            end;
          end else OpPos:=sp-1;
        end else OpPos:=-1;
      until OpPos<=-1;

      // split term and try a recursive call
      if StartPos=-1 then Begin
        AParentNode^.NodeType:=ttntValue;
        AParentNode^.LeftNode:=nil;
        AParentNode^.RightNode:=nil;
        if (ATerm<>'') then Begin
          // if it is a constant, ignore
          t:=GetConstantListIndex(ATerm);
          if t>-1 then Begin
            AParentNode.ValueConstant:=PTMPConstant(FConstantList[t]);
            AParentNode.ValueAssigned:=True;
          end else
            try
              AParentNode.Value:=StrToFloat(ATerm);
              AParentNode.ValueConstant:=nil;
              AParentNode.ValueAssigned:=True;
            except
              ParseError:=mpeInvalidArgument;
              ParseErrorPos:=PosInTerm+TrimCount;
              ParseErrorLength:=length(ATerm);
              result:=false;
              raise EMathParserException.Create(ParseError,ParseErrorPos,ParseErrorLength,ParseErrorDescription);
            end;
        end else Begin
          AParentNode.ValueAssigned:=False;
          AParentNode.ValueConstant:=nil;
          AParentNode.Value:=0;
        end;

      end else Begin
        // split left and right argument
        ALeft:=copy(ATerm,1,StartPos-1);
        ARight:=copy(ATerm,EndPos+1,MaxInt);

        // check, if there is an argument the operator does not accept
        with PTMPOperator(FOperatorList[OpIndex])^ do
          if ((Trim(ALeft)<>'') and (not (mpaLeft in OpArguments)) and (not GetNextOperatorPos(ALeft,t,t,length(ALeft)))) or
             ((Trim(ARight)<>'') and (not (mpaRight in OpArguments)) and (not GetNextOperatorPos(ARight,t,t,length(ARight)))) then Begin
            result:=false;
            ParseError:=mpeUnexpectedArgument;
            // assign error position
            if (not (mpaLeft in OpArguments)) then Begin
              ParseErrorPos:=GetArgumentPos(mpaLeft);
              ParseErrorLength:=length(Trim(ALeft));
            end else Begin
              ParseErrorPos:=GetArgumentPos(mpaRight);
              ParseErrorLength:=length(Trim(ARight));
            end;
            raise EMathParserException.Create(ParseError,ParseErrorPos,ParseErrorLength,ParseErrorDescription);
          end;

        // create subnodes
        New(NewNodeL);
        New(NewNodeR);

        // add subnodes
        AParentNode^.NodeType:=ttntOperator;
        AParentNode^.LeftNode:=NewNodeL;
        AParentNode^.RightNode:=NewNodeR;
        AParentNode^.Value:=-1;
        AParentNode.ValueAssigned:=True;
        AParentNode.ValueConstant:=nil;

        AParentNode.OperatorPos:=PosInTerm+TrimCount+StartPos-1;
        AParentNode^.Operator:=PTMPOperator(FOperatorList[OpIndex]);

        AParentNode^.LeftArgPos:=GetArgumentPos(mpaLeft);
        AParentNode^.LeftArgLength:=length(Trim(ALeft));
        AParentNode^.RightArgPos:=GetArgumentPos(mpaRight)+MPTrimCount(ARight);
        AParentNode^.RightArgLength:=length(Trim(ARight));

        // parse subnodes
        result:=TermToTree(ALeft,NewNodeL,GetArgumentPos(mpaLeft));
        if not result then exit
        else Begin
          result:=TermToTree(ARight,NewNodeR,GetArgumentPos(mpaRight));
          if not result then exit;
        end;
      end;
    end;

    function BracketsIncorrect(ATerm: string): boolean;
    var Count, i: integer;
    Begin
      Count:=0;
      for i:=1 to length(ATerm) do Begin
        // bracket char?
        if ATerm[i]='(' then inc(Count)
        else if ATerm[i]=')' then dec(Count);
        // if there is a bracket to much, error
        if Count<0 then Begin
          result:=True;
          ParseError:=mpeBracketNotOpened;
          ParseErrorPos:=i;
          ParseErrorLength:=1;
          raise EMathParserException.Create(ParseError,ParseErrorPos,ParseErrorLength,ParseErrorDescription);
        end;
      end;
      // check, if all brackets are closed
      if (Count=0) then result:=False
      else Begin
        result:=True;
        ParseError:=mpeClosingBracketExpected;
        ParseErrorPos:=length(ATerm)+1;
        ParseErrorLength:=0;
        raise EMathParserException.Create(ParseError,ParseErrorPos,ParseErrorLength,ParseErrorDescription);
      end;
    end;

begin 
 (* create root node *)
 New(ARootNode);

 (* parse *)
 if (BracketsIncorrect(ATerm)) or (not TermToTree(ATerm,ARootNode,1)) then Begin
   FreeTermTree(ARootNode);
   result:=nil;
 end else result:=ARootNode;
end;

function TMathParser.FormatTerm(ATermTree: PTTermTreeNode): string;
var AError: TMPError;

    function DoFormatTree(ATreeRootNode: PTTermTreeNode): string;
    Begin
       // case node type
       result:='';
       case ATreeRootNode^.NodeType of
         ttntOperator: Begin
           // insert name of operator
           result:=ATreeRootNode.Operator.OpName;
           // insert left argument
           if (mpaLeft in ATreeRootNode.Operator.OpArguments) then Begin
             // when left node is an operator with lower priority, we need brackets
             if (ATreeRootNode^.LeftNode.NodeType=ttntOperator) and
                (ATreeRootNode^.LeftNode.Operator.OpPriority<ATreeRootNode.Operator.OpPriority) then
               result:='('+DoFormatTree(ATreeRootNode^.LeftNode)+') '+result
             else result:=DoFormatTree(ATreeRootNode^.LeftNode)+' '+result;

           end;
           // insert right argument
           if (mpaRight in ATreeRootNode.Operator.OpArguments) then Begin
             // when right node is an operator with lower (or same) priority, we need brackets
             if (ATreeRootNode^.RightNode.NodeType=ttntOperator) and
                (ATreeRootNode^.RightNode.Operator.OpPriority<=ATreeRootNode.Operator.OpPriority) then  // remove = if you don't want unnessasary brackets
               result:=result+' ('+DoFormatTree(ATreeRootNode^.RightNode)+')'
             else result:=result+' '+DoFormatTree(ATreeRootNode^.RightNode);
           end;
         end;
         
         ttntValue: Begin
           if ATreeRootNode.ValueConstant=nil then result:=FloatToStr(ATreeRootNode.Value)
           else result:=ATreeRootNode.ValueConstant.CName;
         end;
       end;
    end;


begin
  if ATermTree=nil then result:=''
  else result:=DoFormatTree(ATermTree);
end;

procedure TMathParser.FreeLastTermTree;
begin
 FreeTermTree(FLastTermTree);
end;

procedure TMathParser.FreeTermTree(ATermTree: PTTermTreeNode);
begin
 // exit if no tree
 if ATermTree=nil then exit;
 // if the tree is the same as FLastTermTree, reset FLastTermTree 
 if (ATermTree=FLastTermTree) then FLastTermTree:=nil;
 // Free recursive
 if ATermTree^.NodeType=ttntOperator then Begin
   if ATermTree^.LeftNode<>nil then FreeTermTree(ATermTree^.LeftNode);
   if ATermTree^.RightNode<>nil then FreeTermTree(ATermTree^.RightNode);
 end;
 Dispose(ATermTree);
 ATermTree:=nil;
end;

function TMathParser.GetConstantListIndex(Name: string): integer;
var i: integer;
begin
 result:=-1;
 for i:=0 to FConstantList.Count-1 do
   if PTMPConstant(FConstantList[i]).CName=lowercase(Name) then Begin
     result:=i;
     break;
   end;
end;

function TMathParser.GetConstants(Name: string): TMPConstant;
var Index: integer;
begin
 Index:=GetConstantListIndex(Name);
 if Index<>-1 then result:=PTMPConstant(FConstantList[Index])^
 else raise Exception.Create('There is no constant named "'+Name+'".');
end;

function TMathParser.GetConstantValues(const Name: String): Extended;
var Index: integer;
begin
 Index:=GetConstantListIndex(Name);
 if Index<>-1 then result:=PTMPConstant(FConstantList[Index])^.CValue
 else raise Exception.Create('There is no constant named "'+Name+'".');
end;

function TMathParser.GetOperatorListIndex(Name: string): integer;
var i: integer;
begin
 result:=-1;
 for i:=0 to FOperatorList.Count-1 do
   if PTMPOperator(FOperatorList[i]).OpName=lowercase(Name) then Begin
     result:=i;
     break;
   end;
end;

function TMathParser.GetOperators(Name: string): TMPOperator;
var Index: integer;
begin
 Index:=GetOperatorListIndex(Name);
 if Index<>-1 then result:=PTMPOperator(FOperatorList[Index])^
 else raise Exception.Create('There is no operator named "'+Name+'".');
end;

function TMathParser.OperatorCount: integer;
begin
 result:=FOperatorList.Count;
end;

function TMathParser.ParseTerm(ATerm: string): PTTermTreeNode;
begin
  (* reset errors *)
  ParseError:=mpeNone;
  ParseErrorPos:=-1;

  (* parse *)
  result:=DoParseTerm(ATerm);

  (* save result temporaly *)
  FLastTermTree:=result;
end;

procedure TMathParser.RegisterStandardOperators;
begin
 AddOperatorEx('+',MPOperatorAddition,[mpaLeft,mpaRight],MPOP_OPERATOR_ADDITION);
 AddOperatorEx('-',MPOperatorSubtraction,[mpaLeft,mpaRight],MPOP_OPERATOR_SUBTRACTION);
 AddOperatorEx('*',MPOperatorMultiplication,[mpaLeft,mpaRight],MPOP_OPERATOR_MULTIPLICATION);
 AddOperatorEx('/',MPOperatorDivision,[mpaLeft,mpaRight],MPOP_OPERATOR_DIVISION);
 AddOperatorEx('sin',MPOperatorSin,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('cos',MPOperatorCos,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('tan',MPOperatorTan,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('sqrt',MPOperatorSqrt,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('^',MPOperatorPower,[mpaLeft,mpaRight],MPOP_OPERATOR_LEFTRIGHT);
 AddOperatorEx('log',MPOperatorLog,[mpaLeft,mpaRight],MPOP_OPERATOR_LEFTRIGHT);
 AddOperatorEx('ln',MPOperatorLn,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('arccos',MPOperatorArcCos,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('arcsin',MPOperatorArcSin,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('arctan',MPOperatorArcTan,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('sinh',MPOperatorSinH,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('cosh',MPOperatorCosH,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('tanh',MPOperatorTanH,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('div',MPOperatorDiv,[mpaLeft,mpaRight],MPOP_OPERATOR_LEFTRIGHT);
 AddOperatorEx('mod',MPOperatorMod,[mpaLeft,mpaRight],MPOP_OPERATOR_LEFTRIGHT);
 AddOperatorEx('abs',MPOperatorAbs,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('floor',MPOperatorFloor,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('ceil',MPOperatorCeil,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('exp',MPOperatorExp,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('!',MPOperatorExp2,[mpaLeft],MPOP_OPERATOR_LEFTONLY);

 AddConstant('pi',3.1415926535897932385);
end;

procedure TMathParser.RegisterExtendedOperators;
begin
 AddOperatorEx('max',MPOperatorMax,[mpaLeft,mpaRight],MPOP_OPERATOR_LEFTRIGHT);
 AddOperatorEx('min',MPOperatorMin,[mpaLeft,mpaRight],MPOP_OPERATOR_LEFTRIGHT);
 AddOperatorEx('arccosh',MPOperatorArcCosh,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('arcsinh',MPOperatorArcsinh,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('arctanh',MPOperatorArctanh,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('arctan2',MPOperatorArctan2,[mpaLeft,mpaRight],MPOP_OPERATOR_LEFTRIGHT);
 AddOperatorEx('cot',MPOperatorCot,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('cotan',MPOperatorCotan,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('hypot',MPOperatorHypot,[mpaLeft,mpaRight],MPOP_OPERATOR_LEFTRIGHT);
 AddOperatorEx('sec',MPOperatorSec,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('random',MPOperatorRandom,[],MPOP_OPERATOR_NONE);
 AddOperatorEx('nsqrt',MPOperatorNsqrt,[mpaLeft,mpaRight],MPOP_OPERATOR_LEFTRIGHT);
 Randomize;
end;

procedure TMathParser.RegisterConversionOperators;
begin
 AddOperatorEx('DegToRad',MPOperatorDeg2Rad,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('DegToGrad',MPOperatorDeg2Grad,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('RadToDeg',MPOperatorRad2Deg,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('RadToCycle',MPOperatorRad2Cycle,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('RadToGrad',MPOperatorRad2Grad,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('GradToRad',MPOperatorGrad2Rad,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
 AddOperatorEx('GradToDeg',MPOperatorGrad2Deg,[mpaRight],MPOP_OPERATOR_RIGHTONLY);
end;

procedure TMathParser.SetConstants(Name: string; const Value: TMPConstant);
var Index: integer;
begin
 Index:=GetConstantListIndex(Name);
 if Index<>-1 then PTMPConstant(FConstantList[Index])^:=Value
 else raise Exception.Create('There is no constant named "'+Name+'".');
end;

procedure TMathParser.SetConstantValues(const Name: String;
  const Value: Extended);
var Index: integer;
begin
 Index:=GetConstantListIndex(Name);
 if Index<>-1 then PTMPConstant(FConstantList[Index])^.CValue:=Value
 else raise Exception.Create('There is no constant named "'+Name+'".');
end;

procedure TMathParser.SetOperators(Name: string; const Value: TMPOperator);
var Index: integer;
begin
 Index:=GetOperatorListIndex(Name);
 if Index<>-1 then PTMPOperator(FOperatorList[Index])^:=Value
 else raise Exception.Create('There is no operator named "'+Name+'".');
end;

procedure TMathParser.SetParseError(const Value: TMPError);
begin
  FParseError := Value;
  case FParseError of
    mpeNone: ParseErrorDescription:='';
    mpeUnknownIdentifier: ParseErrorDescription:='unknown identifier';
    mpeInvalidArgument: ParseErrorDescription:='invalid argument';
    mpeUnexpectedArgument: ParseErrorDescription:='unexpected argument';
    mpeBracketNotOpened: ParseErrorDescription:='bracket not opened';
    mpeClosingBracketExpected: ParseErrorDescription:='closing bracket excepted';
    mpeMissingArgument: ParseErrorDescription:='missing argument';
    mpeDivisionByZero: ParseErrorDescription:='division by zero';
  end;
end;

procedure TMathParser.SetParseErrorDescription(const Value: string);
begin
  FParseErrorDescription := Value;
end;

procedure TMathParser.SetParseErrorLength(const Value: integer);
begin
  FParseErrorLength := Value;
end;

procedure TMathParser.SetParseErrorPos(const Value: integer);
begin
  FParseErrorPos := Value;
end;

{ EMathParserException }

constructor EMathParserException.Create(FError: TMPError; FErrorPos,
      FErrorLength: integer; FErrorDescription: string);
begin
 Error:=FError;
 ErrorPos:=FErrorPos;
 ErrorLength:=FErrorLength;
 ErrorDescription:=FErrorDescription;
end;

{ Functions }

function MPTrim(var AString: string): integer;
Begin
  result:=0;
  (* Left *)
  while (AString<>'') and (AString[1]=' ') do Begin
    Delete(AString,1,1);
    inc(result);
  end;
  (* Right *);
  AString:=TrimRight(AString);
end;
function MPTrimCount(AString: string): integer;
Begin
  result:=0;
  while (AString<>'') and (AString[1]=' ') do Begin
    Delete(AString,1,1);
    inc(result);
  end;
end;

(* standard operators *)

function MPOperatorAddition(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if not (mpaRight in AssignedArguments) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else Begin
    result:=LeftArg + RightArg;
  end;
end;

function MPOperatorSubtraction(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if not (mpaRight in AssignedArguments) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else Begin
    result:=LeftArg - RightArg;
  end;
end;

function MPOperatorMultiplication(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments<>[mpaLeft,mpaRight]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right is argument missing';
  end else Begin
    result:=LeftArg * RightArg;
  end;
end;

function MPOperatorDivision(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments<>[mpaLeft,mpaRight]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right is argument missing';
  end else if RightArg=0 then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeDivisionByZero;
    ErrorDescription:='';
  end else result:=LeftArg / RightArg;
end;

function MPOperatorSin(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=sin(RightArg);
end;

function MPOperatorCos(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=cos(RightArg);
end;

function MPOperatorTan(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else if (cos(RightArg)=0) then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeDivisionByZero;
    ErrorDescription:='right argument out of range';
  end else result:=tan(RightArg);
end;

function MPOperatorSqrt(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else if (RightArg<0) then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeInvalidArgument;
    ErrorDescription:='argument must be greater than or equal 0';
  end else result:=sqrt(RightArg);
end;

function MPOperatorPower(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right argument is missing';
  end else result:=Power(LeftArg,RightArg);
end;

function MPOperatorLn(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if not (mpaRight in AssignedArguments) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument ist missing';
  end else if RightArg<=0 then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeInvalidArgument;
    ErrorDescription:='';
  end else result:=Ln(RightArg);
end;

function MPOperatorLog(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments<>[mpaLeft,mpaRight]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right argument is missing';
  end else if LeftArg=1 then Begin
    InvalidArguments:=[mpaLeft];
    Error:=mpeDivisionByZero;
    ErrorDescription:='division by zero: left argument musn''t be 1';
  end else if LeftArg<=0 then Begin
    InvalidArguments:=[mpaLeft];
    Error:=mpeInvalidArgument;
    ErrorDescription:='left argument must be greater then 0';
  end else result:=LogN(LeftArg,RightArg);
end;

function MPOperatorArcSin(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else if (RightArg<-1) or (RightArg>1) then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeInvalidArgument;
    ErrorDescription:='right argument must be between -1 and 1';
  end else result:=arcsin(RightArg);
end;

function MPOperatorArcCos(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else if (RightArg<-1) or (RightArg>1) then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeInvalidArgument;
    ErrorDescription:='right argument must be between -1 and 1';
  end else result:=arccos(RightArg);
end;

function MPOperatorArcTan(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=arctan(RightArg);
end;

function MPOperatorSinH(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=sinh(RightArg);
end;

function MPOperatorCosH(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=cosh(RightArg);
end;

function MPOperatorTanH(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=tanh(RightArg);
end;

function MPOperatorMod(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right argument is missing';
  end else result:=Trunc(LeftArg) mod Trunc(RightArg);
end;

function MPOperatorDiv(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right argument is missing';
  end else result:=Trunc(LeftArg) div Trunc(RightArg);
end;

function MPOperatorAbs(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=abs(RightArg);
end;

function MPOperatorFloor(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=floor(RightArg);
end;

function MPOperatorCeil(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=ceil(RightArg);
end;

function MPOperatorExp(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=exp(RightArg);
end;

function MPOperatorExp2(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=exp(LeftArg);
end;

(* extended operators *)

function MPOperatorMax(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right argument is missing';
  end else result:=Max(LeftArg,RightArg);
end;

function MPOperatorMin(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right argument is missing';
  end else result:=Min(LeftArg,RightArg);
end;

function MPOperatorArcCosh(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else if (RightArg<1) then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeInvalidArgument;
    ErrorDescription:='right argument must be greater than or equal to 1';
  end else result:=ArcCosh(RightArg);
end;

function MPOperatorArcSinh(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=ArcSinh(RightArg);
end;

function MPOperatorArcTanh(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else if (RightArg<-1) or (RightArg>1) then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeInvalidArgument;
    ErrorDescription:='right argument must be between -1 and 1';
  end else result:=ArcTanh(RightArg);
end;

{y arctan2 x}
function MPOperatorArcTan2(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right argument is missing';
  end else if (RightArg=0) then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeInvalidArgument;
    ErrorDescription:='right argument mustn''t be 0';
  end else result:=ArcTan2(LeftArg,RightArg);
end;

function MPOperatorCot(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=Cot(RightArg);
end;

function MPOperatorCotan(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else if (RightArg=0) then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeInvalidArgument;
    ErrorDescription:='right argument mustn''t be 0';
  end else result:=Cotan(RightArg);
end;

function MPOperatorHypot(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right argument is missing';
  end else result:=Hypot(LeftArg,RightArg);
end;

function MPOperatorSec(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=Sec(RightArg);
end;

{lowerrange random upperrange}
function MPOperatorRandom(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  result:=Random(1000) / 1000 + 0.001;
end;

function MPOperatorNSqrt(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left or right argument is missing';
  end else if (RightArg<0) then Begin
    InvalidArguments:=[mpaRight];
    Error:=mpeInvalidArgument;
    ErrorDescription:='right argument must be greater than or equal 0';
  end else if (LeftArg<0) then Begin
    InvalidArguments:=[mpaLeft];
    Error:=mpeInvalidArgument;
    ErrorDescription:='left argument must be greater than or equal 0';
  end else result:=Power(RightArg,(1 / LeftArg));
end;

(* conversion operators *)

function MPOperatorDeg2Rad(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=DegToRad(RightArg);
end;

function MPOperatorDeg2Grad(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=DegToGrad(RightArg);
end;
                 
function MPOperatorRad2Deg(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=RadToDeg(RightArg);
end;

function MPOperatorRad2Cycle(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=RadToCycle(RightArg);
end;

function MPOperatorRad2Grad(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=RadToGrad(RightArg);
end;

function MPOperatorGrad2Rad(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=GradToRad(RightArg);
end;

function MPOperatorGrad2Deg(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError; var ErrorDescription: string;
  var InvalidArguments: TMPArguments): Extended;
Begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='right argument is missing';
  end else result:=GradToDeg(RightArg);
end;

end.