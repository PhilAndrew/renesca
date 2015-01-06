package renesca.json

import renesca.Query

case class Request(statements:List[Statement] = Nil)

object Statement {
  def apply(query: Query, resultDataContents: String):Statement = {
    apply(query, List(resultDataContents))
  }

  def apply(query:Query, resultDataContents: List[String]):Statement = {
    new Statement(
      query.statement,
      if (query.parameters.nonEmpty) Some(query.parameters) else None,
      Some(resultDataContents)
    )
  }
}

// TODO: parameters can be a lot more than just propertyValues:
// http://neo4j.com/docs/stable/cypher-parameters.html
case class Statement(statement:String, parameters:Option[Map[String,PropertyValue]] = None, resultDataContents:Option[List[String]] = None)
