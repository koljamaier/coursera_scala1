package recfun

class Banes {

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
    //val test1 = test.mapValues(s => s+4)
    //println(test("body"))
  }

}
