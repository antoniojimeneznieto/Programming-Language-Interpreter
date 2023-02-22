package amyc
package analyzer

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        // Variables
        case Variable(name: Name) => topLevelConstraint(env(name))

        // Literals
        case IntLiteral(_) => topLevelConstraint(IntType)

        case StringLiteral(_) => topLevelConstraint(StringType)

        case BooleanLiteral(_) => topLevelConstraint(BooleanType)

        case UnitLiteral() => topLevelConstraint(UnitType)

        // Binary operators
        case Plus(lhs: Expr, rhs: Expr) => topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)

        case Minus(lhs: Expr, rhs: Expr) => topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)

        case Times(lhs: Expr, rhs: Expr) => topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)

        case Div(lhs: Expr, rhs: Expr) => topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)

        case Mod(lhs: Expr, rhs: Expr) => topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)

        case LessThan(lhs: Expr, rhs: Expr) => topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        
        case LessEquals(lhs: Expr, rhs: Expr) => topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        
        case And(lhs: Expr, rhs: Expr) => topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)
        
        case Or(lhs: Expr, rhs: Expr) => topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)

        case Equals(lhs: Expr, rhs: Expr) =>
          // HINT: Take care to implement the specified Amy semantics
          val freshedType = TypeVariable.fresh() 
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, freshedType) ++ genConstraints(rhs, freshedType)
      
        case Concat(lhs: Expr, rhs: Expr) => topLevelConstraint(StringType) ++ genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType)

        // Unary operators
        case Not(e: Expr) => topLevelConstraint(BooleanType) ++ genConstraints(e, BooleanType)

        case Neg(e: Expr) => topLevelConstraint(IntType) ++ genConstraints(e, IntType)

        // Function/constructor call 
        case Call(qname: QualifiedName, args: List[Expr]) =>
          (table.getFunction(qname) ,table.getConstructor(qname)) match {
              case (Some(fct),None) => 
                Constraint(fct.retType, expected, e.position) 
                +: args.zip(fct.argTypes).flatMap((a1, a2) => genConstraints(a1, a2))
              case (None,Some(cstr)) => 
                Constraint(ClassType(cstr.parent), expected, e.position) 
                +: args.zip(cstr.argTypes).flatMap((a1, a2) => genConstraints(a1, a2))
              case _ => throw new scala.MatchError(e)
          }

        // The ; operator
        case Sequence(e1: Expr, e2: Expr) => 
          val freshedType = TypeVariable.fresh()
          topLevelConstraint(freshedType) ++ genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, freshedType)

        // Local variable definition
        case Let(ParamDef(name, typeTree): ParamDef, value: Expr, body: Expr) =>
          val freshedType = TypeVariable.fresh()
          topLevelConstraint(freshedType) ++ genConstraints(value, typeTree.tpe) ++ genConstraints(body, freshedType)(env + (name -> typeTree.tpe))

        // If-then-else
        case Ite(cond: Expr, thenn: Expr, elze: Expr) => genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)

        // Pattern matching
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            pat match
              case WildcardPattern() => (List(), Map())
              case IdPattern(name: Name) =>
                val freshedType = TypeVariable.fresh()
                (List(Constraint(freshedType, scrutExpected, pat.position)), Map(name -> freshedType))
              case LiteralPattern(lit) =>  
                lit match 
                  case IntLiteral(_) => (List(Constraint(IntType, scrutExpected, pat.position)), Map())
                  case StringLiteral(_) => (List(Constraint(StringType, scrutExpected, pat.position)), Map())
                  case BooleanLiteral(_) => (List(Constraint(BooleanType, scrutExpected, pat.position)), Map())
                  case UnitLiteral() => (List(Constraint(UnitType, scrutExpected, pat.position)), Map())
                
              case CaseClassPattern(qname, args) =>
                val pattern = args.zip(table.getConstructor(qname).get.argTypes).map((a1, a2) => handlePattern(a1, a2))
                (Constraint(ClassType(table.getConstructor(qname).get.parent), scrutExpected, pat.position) 
                :: pattern.unzip._1.flatten, pattern.unzip._2.flatMap(_.toList).toMap)

          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))


        // Represents a computational error; prints its message, then exits 
        case Error(msg: Expr) => topLevelConstraint(expected) ++ genConstraints(msg, StringType)

      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          (found, expected) match 
            case (x: TypeVariable, y: TypeVariable) => if (x.id == y.id) solveConstraints(more) else solveConstraints(subst_*(constraints, x.id, y))
            case (x: TypeVariable, _) => solveConstraints(Constraint(expected, found, pos) :: more)
            case (some, y: TypeVariable) => solveConstraints(subst_*(constraints, y.id, some))
            case (some, elze) => if (some == elze) solveConstraints(more) else 
              error("Expected: " ++ expected.toString ++ ", but found: " ++ found.toString, pos)
              solveConstraints(more)
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
