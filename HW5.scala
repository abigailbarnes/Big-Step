// CMSC 22100, Autumn 2022, HW5

import scala.io.Source

enum Token:
  case TLParen
  case TRParen
  case TLBrack  
  case TRBrack
  case TLCurly
  case TRCurly
  case TColon
  case TDot
  case TArrow
  case KW_true
  case KW_false
  case KW_first
  case KW_second
  case KW_not
  case TZero
  case TVar(varName:String) // one or more lowercase letters
  case TTy(typeName:String) // either N for nat or B for bool

enum Type:
  case TyNat
  case TyBool
  case TyCross(tau1:Type,tau2:Type)
  case TyFunc(tau1:Type,tau2:Type)

enum Term:
  case TmTrue
  case TmFalse
  case TmZero
  case TmVar(varName:String)
  case TmAbs(varName:String,varType:Type,body:Term)
  case TmApp(t1:Term,t2:Term)
  case TmPair(t1:Term,t2:Term)
  case TmFirst(t1:Term)
  case TmSecond(t1:Term)
  case TmNot(t1:Term)

import Token.*
import Type.*
import Term.*

// ===== utilities

def unparseTy(tau: Type): String =  tau match {
  case TyNat => "N"
  case TyBool => "B"
  case TyCross(tau1,tau2) => s"{${unparseTy(tau1)} ${unparseTy(tau2)}}"
  case TyFunc(tau1,tau2) => s"[${unparseTy(tau1)} -> ${unparseTy(tau2)}]"
}

def unparse(t: Term): String = t match {
  case TmVar(x) => x
  case TmAbs(x,tau,b) => s"[$x:${unparseTy(tau)}.${unparse(b)}]"
  case TmApp(t1,t2) => s"(${unparse(t1)} ${unparse(t2)})"
  case TmPair(t1,t2) => s"{${unparse(t1)} ${unparse(t2)}}"
  case TmFirst(t1) => s"(first ${unparse(t1)})"
  case TmSecond(t1) => s"(second ${unparse(t1)})"
  case TmTrue => "true"
  case TmFalse => "false"
  case TmZero => "0"
  case TmNot(t1) => s"(not ${unparse(t1)})"
}

def isV(t: Term): Boolean = t match {
  case TmAbs(_,_,_) => true
  case TmPair(t1,t2) if isV(t1) && isV(t2) => true
  case TmTrue | TmFalse | TmZero => true
  case _ => false
}

// ===== scanner

def concat1(c:Char,pr:(String,List[Char])) = pr match {
  case (s,cs) => (s"${c}$s",cs)
}

def gatherChars(test:Char=>Boolean,cs:List[Char]): (String,List[Char]) = cs match {
  case Nil => ("",Nil)
  case c::tl if test(c) => concat1(c,gatherChars(test,tl))
  case _ => ("",cs)
}

def nextToken(cs:List[Char]): Option[(Token,List[Char])] = cs match {
  case Nil => None
  case '('::tl => Some(TLParen,tl)
  case ')'::tl => Some(TRParen,tl)
  case '['::tl => Some(TLBrack,tl)
  case ']'::tl => Some(TRBrack,tl)
  case '{'::tl => Some(TLCurly,tl)
  case '}'::tl => Some(TRCurly,tl)
  case ':'::tl => Some(TColon,tl)
  case '.'::tl => Some(TDot,tl)
  case '-'::'>'::tl => Some(TArrow,tl)
  case '0'::tl => Some(TZero,tl)
  case c::tl if c.isLower => gatherChars(_.isLower,cs) match {
    case ("true",tll) => Some(KW_true,tll)
    case ("false",tll) => Some(KW_false,tll)
    case ("first",tll) => Some(KW_first,tll)
    case ("second",tll) => Some(KW_second,tll)
    case ("not",tll) => Some(KW_not,tll)
    case (x,tll) => Some(TVar(x),tll)
  }
  case c::tl if c.isUpper => gatherChars(_.isUpper,cs) match {
    case (x,tll) => Some(TTy(x),tll)
  }
  case c::tl if c.isWhitespace => nextToken(tl)
  case c::_ => throw new Exception(s"scan error: $c")
}

def scan(code:String): List[Token] =
  def lp(cs:List[Char]): List[Token] = nextToken(cs) match {
    case None => Nil
    case Some(tok,cs) => tok::lp(cs)
  }
  return lp(code.toList)

// ===== parser

def nextTy(toks:List[Token]): Option[(Type,List[Token])] = toks match {
  case Nil => None
  case TTy(t)::tl => t match {
    case "N" => Some(TyNat,tl)
    case "B" => Some(TyBool,tl)
    case _ => throw new Exception(s"unknown type: $t")
  }
  case TLCurly::tl => nextTy(tl) match {
    case Some(tau1,tl1) => nextTy(tl1) match {
      case Some(tau2,TRCurly::tl2) => Some(TyCross(tau1,tau2),tl2)
      case _ => throw new Exception("error parsing pair type (1)")
    }
    case None => throw new Exception("error parsing pair type (2)")
  }
  case TLBrack::tl => nextTy(tl) match {
    case Some(tau1,TArrow::tl1) => nextTy(tl1) match {
      case Some(tau2,TRBrack::tl2) => Some(TyFunc(tau1,tau2),tl2)
      case _ => throw new Exception("error parsing function type (1)")
    }
    case _ => throw new Exception("error parsing function type (2)")
  }
  case _ => throw new Exception(s"error parsing type ${toks.mkString}")
}

def nextTerm(toks:List[Token]): Option[(Term,List[Token])] = toks match {
  case Nil => None
  case TVar(x)::tl => Some(TmVar(x),tl)
  case KW_true::tl => Some(TmTrue,tl)
  case KW_false::tl => Some(TmFalse,tl)
  case TZero::tl => Some(TmZero,tl)
  case TLCurly::tl =>  nextTerm(tl) match {
    case Some(t1,tl1) => nextTerm(tl1) match {
      case Some(t2,TRCurly::tl2) => Some(TmPair(t1,t2),tl2)
      case _ => throw new Exception("parse error: expected rparen enclosing app")
    }
    case _ => throw new Exception(s"parse error: ${toks.mkString}")
  }
  case TLParen::KW_first::tl => Some(nextSub1(TmFirst(_),"first",tl))
  case TLParen::KW_second::tl => Some(nextSub1(TmSecond(_),"second",tl))
  case TLParen::KW_not::tl => Some(nextSub1(TmNot(_),"not",tl))
  case TLParen::tl => nextTerm(tl) match {
    case Some(t1,tl1) => nextTerm(tl1) match {
      case Some(t2,TRParen::tl2) => Some(TmApp(t1,t2),tl2)
      case _ => throw new Exception("parse error: expected rparen enclosing app")
    }
    case _ => throw new Exception(s"parse error: ${toks.mkString}")
  }
  case TLBrack::TVar(x)::TColon::tl => nextTy(tl) match {
    case Some(tau,TDot::tl1) => nextTerm(tl1) match {
      case Some(body,TRBrack::tl2) => Some(TmAbs(x,tau,body),tl2)
      case Some(_,_) => throw new Exception("parse error: missing right bracket after abs")
      case None => throw new Exception("parse error: abs ended after type")
    }
    case Some(_,_) => throw new Exception("parse error: missing dot in abstraction")
    case None => throw new Exception("parse error: abs ended after colon")
  }
  case _ => throw new Exception(s"parse error: ${toks.mkString}")
}

def nextSub1(k: Term=>Term, name: String, toks: List[Token]): (Term,List[Token]) =
  nextTerm(toks) match {
    case Some(t1, TRParen::tl1) => (k(t1), tl1)
    case Some(_,_) => throw new Exception(s"parse error: expected closing paren after $name")
    case None => throw new Exception(s"parse error: $name ended unexpectedly")
  }

def parse(toks: List[Token]): Term = nextTerm(toks) match {
  case None => throw new Exception("not enough program")
  case Some(st,Nil) => st
  case Some(_,_) => throw new Exception("too much program")
}

// ===== type environments

import scala.collection.mutable.Map

// todo: change this type to something other than Int
type TypeEnv = Map[String, Type]

// todo: change this value according to definition of TypeEnv
val emptyTypeEnv: TypeEnv = Map.empty[String, Type]


def lookup(x: String, gamma: TypeEnv): Option[Type] =
  gamma.get(x)

def extend(x: String, tau: Type, gamma: TypeEnv): TypeEnv =
  gamma.update(x, tau) //make sure the overwriting of the value is correct
  return gamma
  //consider .add(x, tau) if the updating is not correct...
  //we need to consider what is being added when

// ===== type checking

def typeOf(gamma: TypeEnv, t: Term): Type =
  t match
    case TmTrue => TyBool
    case TmFalse => TyBool
    case TmZero => TyNat
    case TmVar(x) => 
      lookup(x, gamma) match  
        case Some(ty) => ty
        case _ => throw new Exception("error typeof: variable not found in type environment")
    case TmApp(term1, term2) =>
      var term1type = typeOf(gamma, term1) 
      term1type match 
        case TyFunc(type1, type2) if (typeOf(gamma, term2) == type1) => type2
        case _ => throw new Exception("error typeof: application ill-typed")
    case TmAbs(varname, vartype, bodyterm) => 
      return TyFunc(vartype, typeOf(extend(varname, vartype, gamma), bodyterm))
    case TmNot(x) if(typeOf(gamma, x) == TyBool) => TyBool
    case TmPair(term1, term2)=> TyCross(typeOf(gamma, term1), typeOf(gamma, term2))
    case TmFirst(TmPair(term1, term2)) => typeOf(gamma, term1)
    case TmSecond(TmPair(term1, term2)) => typeOf(gamma,term2)
    case _ => throw new Exception("error typeof: term ill-typed")


// ===== evaluation

def fv(t:Term): Set[String] =
  t match
    case TmVar(varName) => Set(varName)
    case TmAbs(head, type1, body) => 
      body match
        case TmVar(varName) => Set(varName) ++ Set(head) //if the body is a variable
        case _ => Set(head) ++ fv(body) //if the body is a term that can be further reduced
    case TmApp(t1, t2) =>
      fv(t1) ++ fv(t2)
    case TmPair(t1, t2) => 
      fv(t1) ++ fv(t2) //call fv on a term and if that term is a TmVar then it will create a set with it
    case TmFirst(t1) => fv(t1)
    case TmSecond(t1) => fv(t1)
    case TmNot(t1) => fv(t1)
    case _ => throw new Exception("fv error: invalid term input")

var freshVarSeed = 0
def freshVarName(): String =
  val name:String = s"_v$freshVarSeed"
  freshVarSeed += 1
  return name

// rewrite x to s in t1
def subst(x:String,s:Term,t1:Term): Term = 
  t1 match
    case TmVar(varName) =>
      if(varName == x) 
        return s
      else
        return t1
    
    case TmApp(t1, t2) => 
      TmApp(subst(x, s, t1), subst(x, s, t2))

    case TmAbs(stringhead, vartype, termbody) => 
      if(x == stringhead)
        return t1
      else  
        if(fv(s).contains(stringhead))
          var stringheadprime = freshVarName()
          return subst(x, s, TmAbs(stringheadprime, vartype, subst(stringhead, TmVar(stringheadprime), termbody)))
        else
          TmAbs(stringhead, vartype, subst(x, s, termbody))
    case TmTrue => TmTrue
    case TmFalse => TmFalse
    case TmZero => TmZero

    //calling subst for terms for pair, first, second, and not (similar to application)
    //see ED #145
    case TmPair(t1, t2) =>
      TmPair(subst(x, s, t1), subst(x, s, t2))

    case TmFirst(t1) =>
      TmFirst(subst(x, s, t1))

    case TmSecond(t1) => 
      TmSecond(subst(x, s, t1))

    case TmNot(t1) => 
      TmNot(subst(x,s, t1))



def bigStep(t: Term): Term = 
  t match
    case TmTrue => TmTrue
    case TmFalse => TmFalse
    case TmZero => TmZero
    case TmAbs(varName, varType, body) => t
    case TmApp(t1, t2) =>
      bigStep(t1) match
        case TmAbs(varName1, varType1, body1) => 
          subst(varName1, bigStep(t2), body1)
        case _ => throw new Exception("bigStep error: incorrect application inputs")
    case TmPair(t1, t2) =>
      TmPair(bigStep(t1), bigStep(t2))
    case TmFirst(t1) =>
      bigStep(t1) match
        case TmPair(v1, v2) => v1
        case _ => throw new Exception("bigStep error: incorrect pair inputs")
    case TmSecond(t1) => 
      bigStep(t1) match
        case TmPair(v1, v2) => v2
        case _ => throw new Exception("bigStep error: incorrect pair inputs")
    case TmNot(t1) =>
      bigStep(t1) match
        case TmTrue => TmFalse
        case TmFalse => TmTrue
        case _ => throw new Exception("bigStep error: incorrect not input")
    case _ => throw new Exception("bigStep error: term input invalid")

// ===== main program

@main def interpret(codeOrFilename: String): Unit = {
  var code = codeOrFilename
  try {
    code = Source.fromFile(codeOrFilename).getLines.mkString
  } catch {
    case e: java.io.FileNotFoundException => "pass" // do nothing
  }
  println(code)
  println()
  val toks = scan(code)
  val term = parse(toks)
  println("checking the type of the term:")
  try {
    val tau = typeOf(emptyTypeEnv, term)
    println(unparseTy(tau))
    println()
    println("evaluating:")
  } catch {
    case e => {
      println(e)
      println()
      println("evaluating despite type error:")
    }
  }
  try {
    val value = bigStep(term)
    println(unparse(value))
  } catch {
    case e => println(e)
  }
}



@main def test_lookup_and_extend() = 
  extend("x", Type.TyNat, emptyTypeEnv)
  print(s"testing lookup: ${lookup("x", emptyTypeEnv)}\n")
  print(s"testing lookup: ${lookup("y", emptyTypeEnv)}\n")

@main def test_typeOf() = 
  extend("x", Type.TyNat, emptyTypeEnv)
  print(s"testing typeOf: ${typeOf(emptyTypeEnv, TmAbs("x", TyNat, TmVar("x")))}\n")
  print(s"testing typeOf: ${typeOf(emptyTypeEnv, TmApp(TmAbs("x", TyNat, TmVar("x")), TmZero))}\n")
  //print(s"testing typeOf: ${typeOf(emptyTypeEnv, TmApp(TmAbs("x", TyNat, TmVar("x")), TmTrue))}\n")
  print(s"testing typeOf: ${typeOf(emptyTypeEnv, TmAbs("t",TyBool,TmAbs("t",TyNat,TmVar("t"))))}\n")

@main def test_fv() = 
  print(s"testing fv: ${fv(TmApp(TmApp(TmAbs("t",TyBool,TmAbs("f",TyBool,TmVar("t"))),TmVar("A")),TmVar("B")))}\n")
  print(s"testing fv: ${fv(TmApp(TmAbs("a",TyNat,TmAbs("b",TyNat,TmApp(TmAbs("C",TyBool,TmVar("C")),TmApp(TmVar("b"),TmVar("a"))))),TmVar("x")))}\n")
  print(s"testing fv: ${fv(TmAbs("y", TyNat, TmApp(TmVar("y"), TmVar("y"))))}\n")
  print(s"testing fv: ${fv(TmApp(TmApp(TmAbs("t",TyBool,TmAbs("t",TyNat,TmVar("t"))),TmVar("A")),TmVar("B")))}\n")
  print(s"testing fv: ${fv(TmPair(TmAbs("y", TyNat, TmApp(TmVar("y"), TmVar("y"))), TmApp(TmApp(TmAbs("t",TyBool,TmAbs("t",TyNat,TmVar("t"))),TmVar("A")),TmVar("B"))))}\n")

@main def test_parse() = 
  print(s"testing parse: ${parse(List(TLBrack, TVar("x"), TColon, TTy("N"), TDot, TVar("x"), TRBrack))}\n")

@main def test_subst() =
  //pair
  print(s"testing subst: ${subst("x", TmVar("y"), TmPair(TmVar("x"), TmVar("y")))}\n")
  //first
  print(s"testing subst: ${subst("x", TmVar("y"), TmFirst(TmVar("x")))}\n")
  //second
  print(s"testing subst: ${subst("x", TmVar("y"), TmSecond(TmVar("x")))}\n")
  //not
  print(s"testing subst: ${subst("x", TmVar("y"), TmNot(TmVar("x")))}\n")
  //test statement from HW4
  print(s"testing subst: ${subst("x", TmVar("y"), TmAbs("y", TyNat, TmApp(TmVar("x"), TmVar("y"))))}\n")

@main def test_bigStep() = 
  print(s"testing bigStep: ${bigStep(TmApp(TmAbs("x", TyNat, TmVar("x")), TmTrue))}\n")
  print(s"testing bigStep: ${bigStep(TmAbs("x", TyNat, TmVar("x")))}\n")
