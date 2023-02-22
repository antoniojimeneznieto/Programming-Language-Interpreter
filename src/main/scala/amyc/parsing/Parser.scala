package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion._

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
                 with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[Token] = elem(OperatorKind(string))
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ many(definition) ~ opt(expr) ~ kw("end") ~ identifier).map {
    case obj ~ id ~ defs ~ body ~ _ ~ id1 => 
      if id == id1 then 
        ModuleDef(id, defs.toList, body).setPos(obj)
      else 
        throw new AmycFatalError("Begin and end module names do not match: " + id + " and " + id1)
  }

   // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] = 
    abstractClassDefinition | caseClassDefinition | functionDefinition
    
  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] = {
    (
      parameter ~ opt(valueThenDefaultParameters | moreParameters)
    ).map {
      case param ~ None => List(param)
      case param ~ Some(e, dftParam) => ParamDef(param.name, param.tt, e)::dftParam
    }
  }

  lazy val valueThenDefaultParameters: Syntax[(Option[Expr], List[ParamDef])] =
    (
      delimiter("=").skip ~ expr ~ opt(delimiter(",").skip ~ defaultParameters)
    ).map{
      case e ~ Some(dftParam) => (Some(e), dftParam)
      case e ~ None           => (Some(e), Nil)
    }

  lazy val moreParameters: Syntax[(Option[Expr], List[ParamDef])] = recursive {
    (
      delimiter(",").skip ~ parameter ~ opt(valueThenDefaultParameters | moreParameters)
    ).map{
      case param ~ None => (None, List(param))
      case param ~ Some(value, dftParam)   => (None, ParamDef(param.name, param.tt, value)::dftParam)
    }
  }

  lazy val defaultParameters: Syntax[List[ParamDef]] = repsep(defaultParameter, ",").map(_.toList)

  lazy val defaultParameter: Syntax[ParamDef] = 
    (
      identifierPos ~ ":" ~ typeTree ~ "=" ~ expr
    ).map {
      case id ~ _ ~ t ~ _ ~ e => ParamDef(id._1, t, Some(e)).setPos(id._2)
    }

  lazy val parameter: Syntax[ParamDef] = 
    (
      identifierPos ~ ":" ~ typeTree
    ).map {
      case id ~ _ ~ t => ParamDef(id._1, t, None).setPos(id._2)
    }

  // A list of arguments.
  lazy val arguments: Syntax[List[(Option[Name], Expr)]] = {
    (
      expr ~ opt(valueThenArguments | moreArguments)
    ).map {
      case e ~ None => List((None, e))
      case e ~ Some((None, args)) => (None, e)::args 
      case Variable(id) ~ Some(Some(e), args) => (Some(id), e)::args
      case _ => throw new AmycFatalError("Argument name doesn't exist")
    }
  }

  lazy val valueThenArguments: Syntax[(Option[Expr], List[(Option[Name], Expr)])] =
    (
      delimiter("=").skip ~ expr ~ opt(delimiter(",").skip ~ defaultArguments)
    ).map{
      case e ~ Some(dftParam) => (Some(e), dftParam)
      case e ~ None           => (Some(e), Nil)
    }

  lazy val moreArguments: Syntax[(Option[Expr], List[(Option[Name], Expr)])] = recursive {
    (
      delimiter(",").skip ~ expr ~ opt(valueThenArguments | moreArguments)
    ).map{
      case e ~ None => (None, List((None, e)))
      case e ~ Some((None, args)) => (None, (None, e)::args) 
      case Variable(id) ~ Some(Some(e), args) => (None, (Some(id), e)::args)
      case _ => throw new AmycFatalError("Argument name doesn't exist")
    }
  }

  lazy val defaultArguments: Syntax[List[(Option[Name],Expr)]] = repsep(defaultArgument, ",").map(_.toList)

  lazy val defaultArgument: Syntax[(Option[Name], Expr)] = 
    (
      identifier ~ "=" ~ expr
    ).map {
      case id ~ _ ~ e => (Some(id), e)
    }

  lazy val abstractClassDefinition: Syntax[ClassOrFunDef] =
    (
      kw("abstract")  ~
      kw("class")     ~
      identifier
    )
    .map {
      case kw1 ~ kw2 ~ id => AbstractClassDef(id).setPos(kw1)
    }

   lazy val caseClassDefinition: Syntax[ClassOrFunDef] =
    (
      kw("case") ~
      kw("class") ~
      identifier ~
      delimiter("(") ~
      (parameters | epsilon(Nil)) ~
      delimiter(")") ~
      kw("extends") ~
      identifier
    )
    .map {
      case kw1 ~ kw2 ~ id ~ _ ~ param ~ _ ~ kw3 ~ parent => CaseClassDef(id, param, parent).setPos(kw1)
    }

  lazy val functionDefinition: Syntax[ClassOrFunDef] =
    (
      kw("fn") ~
      identifier ~
      delimiter("(") ~
      (parameters | epsilon(Nil)) ~
      delimiter(")") ~
      delimiter(":") ~
      typeTree ~
      delimiter("=") ~
      delimiter("{") ~
      expr ~
      delimiter("}")
    ).map {
      case kw ~ id ~ _ ~ param ~ _ ~ _ ~ retType ~ _ ~ _ ~ body ~ _ => FunDef(id, param, retType, body).setPos(kw)
  }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = (accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  } ~ opt("(" ~ accept(LiteralKind) 
  { 
    case IntLitToken(32) => IntLiteral(32)
    case t => t
  } ~ ")")).map {
    case (prim@TypeTree(IntType)) ~ Some(_ ~ IntLiteral(32) ~ _) => prim
    case TypeTree(IntType) ~ Some(_ ~ IntLiteral(width) ~ _) =>
      throw new AmycFatalError("Int type can only be used with a width of 32 bits, found : " + width)
    case TypeTree(IntType) ~ Some(_ ~ lit ~ _) =>
      throw new AmycFatalError("Int type should have an integer width (only 32 bits is supported)")
    case TypeTree(IntType) ~ None =>
      throw new AmycFatalError("Int type should have a specific width (only 32 bits is supported)")
    case prim ~ Some(_) =>
      throw new AmycFatalError("Only Int type can have a specific width")
    case prim ~ None => prim
  }
  
    // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] = 
    (
      identifierPos ~ opt(delimiter(".").skip ~ identifier)
    ).map {
    case id ~ Some(id2) => TypeTree(ClassType(QualifiedName(Some(id._1), id2))).setPos(id._2)
    case id ~ None => TypeTree(ClassType(QualifiedName(None, id._1))).setPos(id._2)
  }

  lazy val expr: Syntax[Expr] = recursive {
    letOrSequence
  }

  // (1) val, ;
  lazy val letOrSequence = let.up[Expr] | sequence

  lazy val let: Syntax[Let] =
    (
      kw("val") ~ parameter ~ delimiter("=") ~ ifMatchExpr ~ delimiter(";") ~ expr
    ).map {
      case kw ~ paramDef ~ _ ~ value ~ _ ~ body => Let(paramDef, value, body).setPos(kw)
    }

    // A sequence : Expr ; Expr
  lazy val sequence: Syntax[Expr] =
    (
      ifMatchExpr ~ opt(delimiter(";").skip ~ expr)
    ).map {
      case e1 ~ Some(e2) => Sequence(e1, e2).setPos(e1)
      case e ~ None => e
    }

  // (2) if, match
  lazy val ifMatchExpr: Syntax[Expr] = recursive { matchExpr }

  lazy val ifThenElse: Syntax[Expr] =
    (
      kw("if") ~ delimiter("(") ~ expr ~ delimiter(")") ~
                 delimiter("{") ~ expr ~ delimiter("}") ~
      kw("else").skip ~
                 delimiter("{") ~ expr ~ delimiter("}")
      ).map {
        case kw ~ _ ~ condExpr ~ _ ~ _ ~ thenExpr ~ _ ~ _ ~ elseExpr ~ _ => Ite(condExpr, thenExpr, elseExpr).setPos(kw)
      }
  
  lazy val matchExpr: Syntax[Expr] =
    def chainMatch(e: Expr, list: List[Token ~ Seq[MatchCase] ~ Token]): Expr = {
      list match
        case head :: tail => chainMatch(Match(e, head._1._2.toList), tail)
        case Nil => e
    }

    ((binOpExpr | ifThenElse) ~ opt(many1(kw("match").skip ~ delimiter("{") ~ many1(matchCase) ~ delimiter("}")))
      ).map {
      case e ~ None => e
      case e ~ Some(mat) => chainMatch(e, mat.toList).setPos(e)
    }
  
  lazy val matchCase: Syntax[MatchCase] =
    (
      kw("case") ~ pattern ~ delimiter("=>") ~ expr
    ).map {
      case kw ~ pat ~ _ ~ e => MatchCase(pat, e).setPos(kw)
    }

  // (3) to (9) operators
  lazy val binOpExpr: Syntax[Expr] = 
  (
    operators(unaryOpExpr)
    (
      op("*") | op("/") | op("%")  is LeftAssociative,
      op("+") | op("-") | op("++") is LeftAssociative,
      op("<") | op("<=")           is LeftAssociative,
      op("==")                     is LeftAssociative,
      op("&&")                     is LeftAssociative,
      op("||")                     is LeftAssociative
    ) 
      {
        case (l, OperatorToken("+"), r)  => Plus(l,r).setPos(l)
        case (l, OperatorToken("-"), r)  => Minus(l,r).setPos(l)
        case (l, OperatorToken("*"), r)  => Times(l,r).setPos(l)
        case (l, OperatorToken("/"), r)  => Div(l,r).setPos(l)
        case (l, OperatorToken("%"), r)  => Mod(l,r).setPos(l)
        case (l, OperatorToken("<"), r)  => LessThan(l,r).setPos(l)
        case (l, OperatorToken("<="), r) => LessEquals(l,r).setPos(l)
        case (l, OperatorToken("&&"), r) => And(l,r).setPos(l)
        case (l, OperatorToken("||"), r) => Or(l,r).setPos(l)
        case (l, OperatorToken("=="), r) => Equals(l,r).setPos(l)
        case (l, OperatorToken("++"), r) => Concat(l,r).setPos(l) 
      }
  )

  lazy val unaryOpExpr: Syntax[Expr] =
    (
      opt(op("!") | op("-")) ~ simpleExpr
    ).map {
    case None ~ e => e
    case Some(op) ~ e => op match {
      case OperatorToken("!") => Not(e).setPos(e)
      case OperatorToken("-") => Neg(e).setPos(e)
    }
  }

  // A literal expression.
  lazy val literal: Syntax[Literal[_]] =  lit

  val lit: Syntax[Literal[_]] = accept(LiteralKind) {
    case IntLitToken(v) => IntLiteral(v)
    case BoolLitToken(v) => BooleanLiteral(v)
    case StringLitToken(v) => StringLiteral(v)
  }

  lazy val unitLiteral: Syntax[UnitLiteral] = (delimiter("(") ~ delimiter(")")).map {
    case del ~ _ => UnitLiteral().setPos(del)
  }

  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive {
    literalPattern.up[Pattern] | wildPattern.up[Pattern] | classPattern
  }

  lazy val literalPattern: Syntax[Pattern] =
    (
      literal | unitLiteral.up
    ).map {
      case literal => LiteralPattern(literal).setPos(literal)
    }

  lazy val wildPattern: Syntax[Pattern] =
    (
      kw("_")
    ).map {
      case wildCard => WildcardPattern().setPos(wildCard)
    }

  lazy val classPattern: Syntax[Pattern] =
    (
      identifier ~ opt(opt(delimiter(".").skip ~ identifier) ~ delimiter("(") ~ patterns ~ delimiter(")"))
        ).map {
        case id ~ None => IdPattern(id)
        case id ~ Some(None ~ _ ~ pat ~ _) => CaseClassPattern(QualifiedName(None, id), pat)
        case module ~ Some(Some(id) ~ _ ~ pat ~ _) => CaseClassPattern(QualifiedName(Some(module), id), pat)
      }

  lazy val patterns: Syntax[List[Pattern]] = repsep(pattern, ",").map(_.toList) 

  // Simple Expression and calls
  lazy val simpleExpr: Syntax[Expr] = literal.up[Expr] | variableOrCall | unit | error.up[Expr]

  lazy val unit: Syntax[Expr] =
    (
      delimiter("(") ~ opt(expr) ~ delimiter(")")
    ).map {
      case del1 ~ None    ~ del2 => UnitLiteral().setPos(del1)
      case del1 ~ Some(e) ~ del2 => e
    }

  lazy val variableOrCall: Syntax[Expr] =
    (
      identifierPos ~ opt(opt(delimiter(".") ~>~ identifier) ~ delimiter("(") ~ (arguments | epsilon(Nil)) ~ delimiter(")"))
    ).map {
        case id ~ None => Variable(id._1).setPos(id._2)
        case id ~ Some(None ~ _ ~ a ~ _) => Call(QualifiedName(None, id._1), a).setPos(id._2)
        case id ~ Some(Some(id2) ~ _ ~ a  ~ _) => Call(QualifiedName(Some(id._1), id2), a).setPos(id._2)
      }

  lazy val error: Syntax[Error] =
    (kw("error") ~ "(" ~ expr ~ ")").map {
      case pos ~ _ ~ e ~ _ => Error(e).setPos(pos)
  }

  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = true
      debug(program, showTrails)
      false
    }
  }
  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}
