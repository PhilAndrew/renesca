package renesca.graph

object Path {
  def apply(relations: Relation*): Either[String,Path] = {
    if (relations.isEmpty)
      return Left("Path cannot be empty")

    val nodes = relations.flatMap(r => Seq(r.startNode, r.endNode))
    val isPath = nodes.drop(1).dropRight(1).grouped(2).map(l => l.head == l.last).forall(_ == true)
    val origin = relations.head.origin
    val sameOrigin = origin.isInstanceOf[LocalOrigin] && relations.map(_.origin).forall(_.kind == origin.kind)
    // TODO: should be able to handle interrupted paths and mixed directions
    if (!isPath)
      return Left("Relations do not form a path")
    // TODO: type system?
    if (!sameOrigin)
      return Left("Relations have inconsistent origin")

    val distinctNodes = nodes.distinct
    // TODO: match nodes could be allowed in any path?
    val pathNodes = distinctNodes.filter(_.origin.kind == origin.kind)

    Right(new Path(distinctNodes, pathNodes, relations, origin.asInstanceOf[LocalOrigin]))
  }
}

class Path private[graph](val allNodes: Seq[Node],
                          val nodes: Seq[Node],
                          val relations: Seq[Relation],
                          val origin: LocalOrigin
                           ) extends SubGraph {

  override def toString = s""""Path(${relations.mkString(",")})"""
}