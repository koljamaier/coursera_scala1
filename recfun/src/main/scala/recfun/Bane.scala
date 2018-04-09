package recfun

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
      val t = test.get("test")
      println(t)
      val parsedJson = mapper.readValue(jsonString,classOf[Map[String, Object]])
      println(parsedJson)
      val p = parsedJson("body")
      println(parsedJson("body"))


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
      for ((k,v) <- m1) printf("key: %s, value: %s\n", k, v)

     // for ((k,v) <- test) printf("key: %s, value: %s\n", k, v)

      m1 foreach (x => println (x._1 + "-->" + x._2))
      m1 foreach {case (key, value) => println (key + "-->" + value)}


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

}
