# Big-Step
Correction (10/31): The return type of extend in the code framework is TypeEnv. I inadvertently left it out of the code. (Scala infers return types of functions if it needs to.)

HW5 will give you practice with typing environments and big-step semantics. The framework is below, and the parts you need to complete are marked with "todo", as before.

You can see what the language contains by reading the Term definition.

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

Corresponding to these terms, we have the following grammar of types:

enum Type:
  case TyNat
  case TyBool
  case TyCross(tau1:Type,tau2:Type)
  case TyFunc(tau1:Type,tau2:Type)
Note that the term grammar includes types (in the TmAbs form), so for the first time in any of our homework exercises, we have a concrete syntax for types as well as a concrete syntax for terms.

The scanner and parser for this language are both completely done for you, so you only need to know the concrete syntax in order to write programs in this language. The concrete syntax of types is N for Nat, B for Bool, {tau tau} for product types, and [tau -> tau] for function types.

The concrete syntax of terms is true, false, and 0 for constants, one or more lowercase letters for variables names, excluding keywords, [x:tau.t] for abstractions, (t t) for applications, {t t} for pairs, (first t) and (second t) for pair selection, and (not t) for boolean negation.

If you look for occurrences of "todo" in the file, you will see your tasks are as follows:

The type synonym TypeEnv must be redefined so that it is a data structure capable of representing a typing environment. The type Int is standing in only so the code will compile, but Int is certainly not the type you should use for this purpose. You have some latitude for how you define TypeEnv; think of the different suitable data structures we talked about in lecture. Once you've chosen a TypeEnv representation, define emptyTypeEnv and the operations lookup and extend accordingly.

The function typeOf needs to check the types of terms in the term grammar. The typing rules are a combination of the simply-typed lambda calculus rules in Chapter 9 (see Figure 9-1), as well as the standard rules for boolean operations, pairs, etc. that we have seen many times by now. Note that the empty typing environment is the starting environment when we begin type checking a term, and that environment grows (via the extend operation) as the term is recursively inspected during typechecking.

The function bigStep is an evaluation function that computes the final value of a term in one "big step." The idea of a big-step semantics is introduced in the text in 3.5.17. While a small-step evaluation semantics is good for writers of proofs, a big-step evaluation semantics is good for writers of code, and it describes the kind of evaluation one is more likely to find in real intepreters (as opposed to step-by-step evaluation). The function bigStep consumes a Term and produces a Term (not an Option[Term]), and it is reflexive on values; for example, bigStep(TmTrue) is TmTrue, not None as it would be in the standard small-step function. Please see below for a full specification of big-step rules for evaluating terms in this language.

Finally, in order to perform substitution during evaluation of applications, you will need  to implement fv and subst more or less as before. These should behave basically as they did in HW4, but they both need to be extended beyond the lambda calculus to include the various other term forms present in this language.
