// Problem: Bei der normalen Anwendung von sum könnte es zum
// StackOverflow kommen, weil die Range groß ist (a,b)
// und deshalb sehr viele Calls gemacht werden
// Stattdessen: Benutze die Loop Fkt. innerhalb

def sum(f: Int => Int)(a:Int, b:Int): Int = {
  def loop(a:Int, acc:Int): Int = {
    if(a>b) 0
    else a + loop(a+1,f(a)+acc)
  }
  loop(a,0)
}
sum(x=>x)(0,3)

// This function should return the product of
// all numbers in the interval.
// e.g. product(a,b) = f(a)*(f(a+1))*(f(a+2))*...*(f(b-1))*f(b)
// f is an additional function that specifies the product
def product(f: Int => Int)(a:Int,b:Int) : Int = {
  if(a>b) 1
  else f(a)*product(f)(a+1,b)
}

val tes = product(x=>x*x*x)(1,3)

// write fact as a product
// fact(5)=5*4*3*2*1=1*2*3*4*5
// product(x=>x)(1,5)=fact(5)
def fact(x: Int): Int = {
  product(x=>x)(1,x)
}

fact(4)

// Eine generelle Fkt. schreiben, welche sum und product generalisiert
def general(f: Int => Int, op: String)(a:Int, b:Int) : Int = {
  if(op=="+")
    if(a>b) 1
    else f(a)+general(f,op)(a+1,b)
  else if(op=="*")
    if(a>b) 1
    else f(a)*general(f,op)(a+1,b)
  else 0
}


val str = "k"
str.toList.tail
str.charAt(0)

val str1: List[Char] = "test".toList

str1.toString.indexOf('e')

str1.mkString.indexOf('e')

str1.mkString

str1.toString

for(ch <- str1) yield ch

"test".indexOf('e')

str1.indexOf("e")



val jsonString =
  """
    |{"body":{
    |    "method":"string",
    |    "events":"string",
    |    "clients":"string",
    |    "parameter":"string",
    |    "channel":"string",
    |    "metadata":{
    |        "meta1":"string",
    |        "meta2":"string",
    |        "meta3":"string"
    |    }
    |},
    |"timestamp":"string"}
  """.stripMargin

//val jsonMap = parse(jsonString).values.asInstanceOf[Map[String, Any]]

import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.databind.ObjectMapper


/////
val data = """
    {"some": "json data"}
"""

val mapper = new ObjectMapper

mapper.registerModule(DefaultScalaModule)
val test = mapper.readValue(jsonString, classOf[Map[String, String]])
test.foreach(println)
//val test1 = test.mapValues(s => s+4)
//println(test("body"))
/////

// m.get(2).flatMap{ case m2: Map[Int, _] => m2.get(3) }
str.toList(str.indexOf("k"))

str.zipWithIndex.filter(_._2 != str.indexOf("k")).map(_._1).isEmpty


val te = 15

te % 21

21 % te

val l1 = List(1)

l1.tail
l1.head