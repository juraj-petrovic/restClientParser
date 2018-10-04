import java.nio.charset.StandardCharsets
import java.nio.file._
import fastparse.core.Parsed.{Failure, Success}
import collection.JavaConverters._

/**
  * Created by Jupe on 2.10.2018.
  */
object RestClientParser extends App {
  import fastparse.all._
  val fileText = Files.readAllLines(Paths.get("./resources/new.rest"), StandardCharsets.UTF_8).asScala.mkString("\n")
  //println(fileText)
  case class Variable(name: String, value: String)
  case class BlockName(name: String)
  case class CallId(id: String)
  case class Call(method: HttpMethod, url: Seq[Expression])
  case class Header(name: String, url: Seq[Expression])
  case class HttpMethod(method: String)
  sealed trait Expression extends Product
  case class VariableExpression(exp: String) extends Expression
  case class Constant(const: String) extends Expression
  case class Body(body: Seq[Seq[Expression]])
  trait Block
  case class GlobalVariablesBlock(blockName: BlockName, globalVariables: Seq[Variable]) extends Block
  case class TestCaseBlock(blockName: BlockName, callId: Option[CallId], globalVariables: Seq[Variable], call: Call, headers: Seq[Header], body: Option[Body]) extends Block
  case class TestFile(globalVariables: Seq[Variable], blocks: Seq[Block])
  val ws0 = P(" ").rep
  val ws = P(" ").rep(1)
  val nl = P("\n").rep(1)
  val comment = P((("#" ~ !"##" ~ !" @name") | ("//" ~ !" @name")) ~ (!"\n" ~ AnyChar).rep)
  val emptyLine = P(comment | nl).log()

  val blockName = P("###" ~ (!"\n" ~ AnyChar.!).rep ~ nl).map(x => BlockName(x.mkString(""))).log()
  val callId = P(("# @name " | "// @name")  ~/  CharsWhile(_ != '\n').! ~ nl).map(CallId)
  val variables = P("@" ~/  CharsWhile(_ != '=').! ~/ P("=") ~/ CharsWhile(_ != '\n').! ~ nl).map(Variable.tupled).log().rep
  val variable = P("{{" ~/ CharsWhile(_ != '}').! ~ "}}").!.map( x => VariableExpression(x))
  val constant = P((!"{{" ~ !"\n" ~ AnyChar.!).rep(min = 1).!).map( x => Constant(x))
  val constantNoSpaces = P(CharsWhile(c => c != '\n' && c != ' ').!).map( x => Constant(x))
  val stringExpression = P(variable | constantNoSpaces).rep //no spaces
  val jsonStringExpression = P(variable.log() | constant.log()).rep.log() //no spaces
  val bodyStringExpressionLine = P(!("###" | End | comment) ~ P(variable.log() | constant.log()).rep ~ nl.rep)
  val method = P("GET" | "POST" | "PUT" | "DELETE").!.map(HttpMethod)
  val protocol = P("HTTP/1.1")
  val call = P( method ~ ws ~/ stringExpression ~/ ws.? ~/ protocol.? ~/ nl.?).map(Call.tupled)
  val headerFirstPart = P(CharsWhileIn(('A' to 'Z') ++ ('a' to 'z') :+ '-')).!.log()
  val header = P(headerFirstPart.! ~ ":" ~ ws ~ jsonStringExpression ~ nl).log().map(Header.tupled)
  val body = P(bodyStringExpressionLine.log().rep ~ &("###" | End | comment)).map(Body).log()
  val globalVariablesBlock = P(blockName ~ emptyLine.rep ~ variables ~ emptyLine.rep ~ !method ~ &("###")).map(GlobalVariablesBlock.tupled).log()
  val testCaseBlock = P(blockName ~ emptyLine.rep ~ callId.? ~ emptyLine.rep ~ variables ~ emptyLine.rep ~ call ~ header.rep ~ emptyLine.rep.log() ~ body.log().? ~ emptyLine.rep).map(TestCaseBlock.tupled).log()
  val testFile = P(emptyLine.rep() ~ variables ~ emptyLine.rep ~ (globalVariablesBlock | testCaseBlock).rep() ~ End).map(TestFile.tupled)
  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )

    val x = testFile.parse(fileText)

    x match {
      case a@Success(x,y) => println("Success:" +  x)
      case a@Failure(lastParser,index, extra) => println(x)
       // println(""+ a + " " + extra.traced.toString)
    }

}
