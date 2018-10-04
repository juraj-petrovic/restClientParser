import java.nio.charset.StandardCharsets
import java.nio.file._
import fastparse.core.Parsed.{Failure, Success}
import collection.JavaConverters._

/**
  * Created by Jupe on 2.10.2018.
  */
object RestClientParser extends App {
  import fastparse.all._

  //val file = Files.readAllLines(Paths.get("./resources/member-aeroplan-cycles.rest"), StandardCharsets.UTF_8).asScala.mkString("\n")
  val fileText = Files.readAllLines(Paths.get("./resources/mini.rest"), StandardCharsets.UTF_8).asScala.mkString("\n")
  //println(fileText)
  case class GlobalVariable(name: String, value: String)
  case class CallName(name: String)
  case class CallId(id: String)
  case class Call(method: HttpMethod, url: Seq[Expression])
  case class Header(name: String, url: Seq[Expression])
  case class HttpMethod(method: String)
  sealed trait Expression extends Product
  case class VariableExpression(exp: String) extends Expression
  case class Constant(const: String) extends Expression
  case class Body(body: Seq[Seq[Expression]])
  case class TestCase(callName: CallName, callId: Option[CallId], call: Call, headers: Seq[Header], body: Option[Body])
  case class TestFile(globalVariables: Seq[GlobalVariable], testCases: Seq[TestCase])
  val ws0 = P(" ").rep
  val ws = P(" ").rep(1)
  val nl = P("\n").rep(1)
  val emptyLine = (nl)

  val callName = P("###" ~/  CharsWhile(_ != '\n').! ~ nl).map(CallName)
  val callId = P("# @name " ~/  CharsWhile(_ != '\n').! ~ nl).map(CallId)
  val globalVariables = P("@" ~/  CharsWhile(_ != '=').! ~/ P("=") ~/ CharsWhile(_ != '\n').! ~ nl).map(GlobalVariable.tupled).rep
  val variable = P("{{" ~/ CharsWhile(_ != '}').! ~ "}}").!.map( x => VariableExpression(x))
  val constant = P((!"{{" ~ !"\n" ~ AnyChar.!).rep(min = 1).!).map( x => Constant(x))
  val constantNoSpaces = P(CharsWhile(c => c != '\n' && c != ' ').!).map( x => Constant(x))
  val stringExpression = P(variable | constantNoSpaces).rep //no spaces
  val jsonStringExpression = P(variable.log() | (!"{{" ~ constant.log())).rep.log() //no spaces
  val bodyStringExpressionLine = !"###" ~ P(variable | ( !"{{" ~ constant)).rep ~ nl
  val method = P("GET" | "POST" | "PUT" | "DELETE").!.map(HttpMethod)
  val protocol = P("HTTP/1.1")
  val call = P( method ~/ ws ~/ stringExpression ~/ ws ~/ protocol ~/ nl.?).map(Call.tupled)
  val headerFirstPart = P(CharsWhileIn(('A' to 'Z') ++ ('a' to 'z') :+ '-')).!.log()
  val header = P(headerFirstPart.! ~ ":" ~ ws ~ jsonStringExpression).log().map(Header.tupled)
  val body = P(bodyStringExpressionLine.rep ~ &("###")).map(Body)
  val testCase = P(callName ~ emptyLine.? ~ callId.? ~ emptyLine.? ~ call ~ header.rep(sep = "\n") ~ emptyLine.? ~ body.? ~ emptyLine.?).map(TestCase.tupled)
  val testFile = P(emptyLine.rep() ~ globalVariables ~ emptyLine.rep() ~ testCase.rep() ~ End).map(TestFile.tupled)
  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )

    val x = testFile.parse(fileText)

    x match {
      case a@Success(x,y) => println(x)
      case a@Failure(lastParser,index, extra) =>
        println(""+ a + " " + extra.traced.toString)
    }

}
