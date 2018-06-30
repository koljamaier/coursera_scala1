package recfun

import com.fasterxml.jackson.databind.JsonNode
import scala.collection.JavaConversions._

import scala.util.parsing.json.JSON

object Bane {

    def main(args: Array[String]) {
      val jsonString =
        """
          |{"body":{
          |    "method":"string",
          |    "events":"string",
          |    "clients":"string",
          |    "parameter":"string",
          |    "channel":"string",
          |    "metadata":[{
          |        "meta1":"stri1ng",
          |        "meta2":"string",
          |        "meta3":"string",
          |        "lol" :[{
          |              "zack": "bumm"
          |        },
          |        {
          |         "zack": "bumm232"
          |        }
          |        ]
          |    },
          |    {
          |        "meta1":"dasd",
          |        "meta2":"ddd",
          |        "meta3":"stridng",
          |        "lol" :[{
          |              "zack": "bumm"
          |        },
          |        {
          |         "zack": "bumm232"
          |        }
          |        ]
          |    }
          |    ],
          |    "tester": {
          |      "test1": "dda",
          |      "tee": {"tetl": "b", "waku": "j"}
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

      ////////////// PARSING WITH JACKSON
      val mapper = new ObjectMapper
      mapper.registerModule(DefaultScalaModule)

      val test = mapper.readValue(jsonString, classOf[Map[String, String]])
      //test.foreach(x => println(x + "neuer print"))
      //println(test) // Map(body -> Map(method -> string, parameter -> string, channel -> string, clients -> string, metadata -> Map(meta1 -> string, meta2 -> string, meta3 -> string), events -> string), timestamp -> string)
      //println(test("body"))
      val t = test.get("test")
      //println("hier kommt test.get('test')")
      //println(t) // None
      val parsedJson = mapper.readValue(jsonString,classOf[Map[String, Object]])
      //println(parsedJson)



      val rootNode: JsonNode = mapper.readTree(jsonString)
      println("sizeeee  " + rootNode.size) // 2 -> "body" und "timestamp"
      println("size " + rootNode.get("body").size) // 7


      // Da ist das Ding :_)
      val b = for(i <- rootNode.findValue("tester").fields()) yield {
        i.getKey -> mapper.writeValueAsString(i.getValue)
      }


      val bMap = b.toMap

      println(bMap("test1") + "   " + mapper.readValue(bMap("tee"), classOf[Map[String, String]]))

      println(bMap("tee"))


      def parseJsonToListMap(root: JsonNode, key: String): List[Map[String,String]] = {
        val jNode = root.findValue(key)
        if(jNode.isArray) parseJsonArrayToListMap(jNode.elements.toList)
        else List(extractMap(jNode))
      }

      def extractMap(root: JsonNode): Map[String,String] = {
       val l = for(i <- root.fields()) yield {
          i.getKey -> mapper.writeValueAsString(i.getValue)
        }
        l.toMap
      }

      def parseJsonArrayToListMap(jsonArray: List[JsonNode]): List[Map[String,String]] = {
        println("Hier angekommen")
        jsonArray match {
          case x :: xs => extractMap(x) :: parseJsonArrayToListMap(xs)
          case List() => List()
          //case List() => List(extractMap(jsonArray))
        }
      }


      println("Test Funcs")
      val parsed = parseJsonToListMap(rootNode, "tester")
      println(bMap("tee"))
      println(parsed.head("tee"))

      val parsed1 = parseJsonToListMap(rootNode, "metadata")
      println(parsed1.head("meta1"))
      println(parsed1.tail.head("meta1"))

      println("End Test")



      val phoneNumbersNode1 = rootNode.findValues("lol") // List[JsonNode]
      //println(phoneNumbersNode1 + "test123")


      val personalInformationNode = rootNode.get("body")
      val personalInformationMap : Map[String,String] = mapper.convertValue(personalInformationNode, classOf[Map[String,String]])
      println("test"+ personalInformationMap)
      println("testtiii"+personalInformationMap("metadata"))
      println(personalInformationMap.get("parameter"))
      println("hört hört" + personalInformationMap.get("metadata"))

      val personalInformationNode2 = rootNode.elements()

      println("gier"+rootNode.elements)
      for(i <- personalInformationNode2) {
        println(i)
      }

      println("for fieldNames")
      for(i <- rootNode.fieldNames()) {
        println("tata" + i)
      }


      println("for fields")
      val zz1 = for(i <- rootNode.fields()) {
        if(i.getKey=="body") println(mapper.writeValueAsString(i.getValue))
        if(i.getKey=="body") println(i.getValue.isArray)
      }





      //zz1.foreach(println)

      println("ok")
      val phoneNumbersNode3 : JsonNode = rootNode.findValue("metadata")

      val z = phoneNumbersNode3.elements.toList
      val y = z match {
        case x :: y => println(mapper.writeValueAsString(x))
      }


      println(z)
      //phoneNumbersNode3.fields.foreach(println)
      val tt: Map[String,String] = mapper.convertValue(phoneNumbersNode3.elements.next, classOf[Map[String,String]])
      //println(phoneNumbersNode3.elements().next().mapper.asInstanceOf[Map[String,String]])
      println(tt)
      println(tt("lol"))
      println("ok")

      rootNode.fieldNames().foreach(println(_))




      personalInformationMap.foreach { a: (String, String) =>
        //println(s"$a")
      }


      println("done")

      //println(phoneNumbersNode1.get(0).findValuesAsText("lol"))
      val phoneNumbersNode : JsonNode = rootNode.findValue("lol")
      println(phoneNumbersNode)


      val notexist = rootNode.path("bla")
      if(notexist.isMissingNode) println("Missing Node")

      val p: Object = parsedJson("body")
      println(parsedJson("body"))


      /*
      //Für ein JSON Array
      public Iterator<JsonNode> readPhoneNumbers(){
     JsonNode phoneNumbersNode = rootNode.path("phoneNumbers");
     Iterator<JsonNode> elements = phoneNumbersNode.elements();
     while(elements.hasNext()){
         JsonNode phoneNode = elements.next();
         logger.info("\n----------------------------\nPhone Numbers = "+phoneNode.asLong());
     }
     return elements;
 }
       */

      test match {
        case e: Map[String, Any] => {
          //println(e) //output: Map(name -> OZKA, monthRevenue -> 1000.75, developer -> true, birthDate -> 1981-02-08T20:00:00.000Z, id -> 1.0)
          //e.foreach { pair =>
          //  println(pair._1 + ":" + pair._2)
          //}
          //e.foreach { case (key: String, value: String) => println(value) }
        }
        case _ => println("Failed.")
      }

      //////////// PARSING WITH SCALA JSON UTIL




      //

      println("testAnfang")

      val jsonMap = JSON.parseFull(jsonString).getOrElse(0).asInstanceOf[Map[String,String]]
      val innerMap = jsonMap("body").asInstanceOf[Map[String,String]]
      innerMap.keys //will give keys
      innerMap("metadata") //will give value for any key anykey



      val result = JSON.parseFull(jsonString)
      result match {
        // Matches if jsonStr is valid JSON and represents a Map of Strings to Any
        case Some(map: Map[String, Any]) => println(map)
        case None => println("Parsing failed")
        case other => println("Unknown data structure: " + other)
      }








      println("testEnde")
      //

      case class Tweet(user: String, text: String, retweets: Int)

      import scala.util.parsing.json._

      def getList[T](s: String): List[T] =
        JSON.parseFull(s).get.asInstanceOf[List[T]]

      def getMap(s: String): Map[String, Any] =
        JSON.parseFull(s).get.asInstanceOf[Map[String, Any]]


      /*def getMppss(json: String): List[Map[String,String]] =
        for (map <- getList[Map[String, String]](json)) yield {
          map
        }*/

      //val zZ = getList[Map[String, String]](jsonString)
      val pp = JSON.parseFull(jsonString).get.asInstanceOf[Map[String,Any]]
      //val pp1 = JSON.parseFull(jsonString).get.asInstanceOf[List[Map[String,Any]]]

      println(pp("body"))
      //println("bllaslslalsalsa")

      def getTweetData(user: String, json: String): List[Tweet] = {
        // is list
        val l = getList[Map[String, Any]](json)
        for (map <- l) yield {
          val text = map("text")
          val retweets = map("retweets")
          new Tweet(user, text.toString, retweets.toString.toDouble.toInt)
        }
      }



      val tweets = JSON.parseFull(jsonString).get.asInstanceOf[Map[String,Any]]
      //println("HIER")
      //println(tweets("body").asInstanceOf[Map[String,Any]]("method"))
      //val tweeL = getList[Map[String, Any]](jsonString)
      //println(tweeL)

      /*jsonString.flatMap(record => {
        try {
          Some(mapper.readValue(record, String))
        } catch {
          case e: Exception => None
        }
      })*/

      //val jsonMap = parse(jsonString).values.asInstanceOf[Map[String, Any]]

      //val temp = test.get("body").flatMap{case m: String => m.get(3)}

      //t.get(2).flatMap{ case m2: Map[Int, _] => m2.get(3) }

      //val t1 = test.asJava
      //val t2 = test.asScala
      //val test1 = test.mapValues(s => s+4)
      //println(test("body"))

      //////////////
      val m1 = Map("fname" -> "Al", "lname" -> "Alexander")
      //for ((k,v) <- m1) printf("key: %s, value: %s\n", k, v)

     // for ((k,v) <- test) printf("key: %s, value: %s\n", k, v)

      //m1 foreach (x => println (x._1 + "-->" + x._2))
      //m1 foreach {case (key, value) => println (key + "-->" + value)}


      val p1Ratings = Map(
        "Lady in the Water"-> 3.0,
        "Snakes on a Plane"-> 4.0,
        "You, Me and Dupree"-> 3.5
      )

      val p2Ratings = Map(
        "Lady in the Water"-> 3.0,
        "Snakes on a Plane"-> 4.0,
        "You, Me and Dupree"-> 3.5
      )

      val similarItems = scala.collection.mutable.Map[String, Boolean]()


      p1Ratings.keys.foreach( (movie) =>
        if (p2Ratings.contains(movie)) similarItems += (movie -> true)
      )

      p1Ratings foreach ( (movie) =>
        if (p2Ratings.contains(movie._1)) similarItems += (movie._1 -> true)
        )

      for ((movie1, rating1) <- p1Ratings) {
        if (p2Ratings.contains(movie1)) similarItems += (movie1 -> true)
      }



    }

  val request =
    """
      |"{"id":1,"name":"OZKA","birthDate":"1981-02-08T20:00:00.000Z","monthRevenue":1000.75,"developer":true}"
    """.stripMargin
  //println(request)//{"id":1,"name":"OZKA","birthDate":"1981-02-08T20:00:00.000Z","monthRevenue":1000.75,"developer":true}
  val result1: Option[Any] = scala.util.parsing.json.JSON.parseFull(request)
  //klappt nicht!
  //val result: Map[String, Any] = JSON.parseFull(request).get.asInstanceOf[Map[String,Any]]
  //println(result1)


  result1 match {
    case Some(e: Any) => {
     // println("das ist die Map")
      //println("das ist die map"+ e) //output: Map(name -> OZKA, monthRevenue -> 1000.75, developer -> true, birthDate -> 1981-02-08T20:00:00.000Z, id -> 1.0)
      //e.foreach { pair =>
      //  println(pair._1 + ":" + pair._2)
      //}
      //e.foreach { case (key: String, value: Any) => println(value) }
      //println(e)
    }
    case None => println("Failed.")
  }




  //println("ok")


  class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }

  object M extends CC[Map[String, Any]]
  object L extends CC[List[Any]]
  object S extends CC[String]
  object D extends CC[Double]
  object B extends CC[Boolean]

  val jsonString =
    """
      {
        "languages": [{
            "name": "English",
            "is_active": true,
            "completeness": 2.5
        }, {
            "name": "Latin",
            "is_active": false,
            "completeness": 0.9
        }]
      }
    """.stripMargin

  val res = for {
    Some(M(map)) <- List(JSON.parseFull(jsonString))
    L(languages) = map("languages")
    M(language) <- languages
    S(name) = language("name")
    B(active) = language("is_active")
    D(completeness) = language("completeness")
  } yield {
    (name, active, completeness)
  }

  assert( res == List(("English",true,2.5), ("Latin",false,0.9)))


}
