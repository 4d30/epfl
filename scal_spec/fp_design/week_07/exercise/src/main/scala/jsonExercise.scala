package main.scala

object jsonExercise:

  enum JSON:
    case Seq (elems: List[JSON])
    case Obj (bindings: Map[String, JSON])
    case Num (num: Double)
    case Str (str: String)
    case Bool(b: Boolean)
    case Null

  def show(json: JSON): String = json match
    case JSON.Seq(elems) =>
      elems.map(show).mkString("[",",","]")
    case JSON.Obj(bindings) =>
      val assocs = bindings.map(
        (key, value) => s"${inQuotes(key)}: ${show(value)}")
      assocs.mkString("{",",\n","}")
    case JSON.Num(num) => num.toString
    case JSON.Str(str) => inQuotes(str)
    case JSON.Bool(b)  => b.toString
    case JSON.Null     => "null"
    
  
  def bindings(x: JSON): List[(String, JSON)] = x match
    case JSON.Obj(bindings) => bindings.toList
    case _ => Nil

  def inQuotes(str: String): String = "\"" + str + "\""

  val jsData = JSON.Obj(Map(
    "firstname"->JSON.Str("John"),
    "lastname" -> JSON.Str("Smith"),
    "address" -> JSON.Obj(Map(
      "streetAddress" -> JSON.Str("21 2nd Street"),
      "state" -> JSON.Str("NY"),
      "postalCode" -> JSON.Num(10021)
    )),
    "phoneNumbers" -> JSON.Seq(List(
      JSON.Obj(Map(
        "type" -> JSON.Str("home"), "number" -> JSON.Str("212 555 1234")
      )),
      JSON.Obj(Map(
        "type" -> JSON.Str("work"), "number" -> JSON.Str("212 555 9999")
      )),
      JSON.Obj(Map(
        "type" -> JSON.Str("fax"), "number" -> JSON.Str("646 555 4567")
      )) )) ))

  def main(args: Array[String]): Unit =
    println(show(jsData))
    println(bindings(jsData))
    val numbers = { 
    for
      case ("phoneNumbers",JSON.Seq(numberInfos)) <- bindings(jsData)
      numberInfo <- numberInfos
      case ("number", JSON.Str(number)) <- bindings(numberInfo)
      if number.startsWith("212")
    yield
        number}
    println(numbers)

