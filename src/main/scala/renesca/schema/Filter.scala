package renesca.schema

import renesca.{graph => raw}
import scala.collection.mutable

trait Filter {
  def graph: raw.Graph

  def filterNodes[T <: Node](nodes: Traversable[raw.Node], nodeFactory: NodeFactory[T]): mutable.LinkedHashSet[T] = {
    mutable.LinkedHashSet.empty ++ nodes.filter(_.labels.contains(nodeFactory.label)).map { node =>
      val schemaNode = nodeFactory.wrap(node)
      schemaNode.graphOption = Some(graph)
      schemaNode
    }
  }

  def filterRelations[START <: Node, RELATION <: Relation[START, END], END <: Node]
  (relations: Traversable[raw.Relation], relationFactory: RelationFactory[START, RELATION, END]): mutable.LinkedHashSet[RELATION] = {
    mutable.LinkedHashSet.empty ++ relations.filter(_.relationType == relationFactory.relationType).map(relationFactory.wrap)
  }

  def filterHyperRelations[
  START <: Node,
  STARTRELATION <: Relation[START, HYPERRELATION],
  HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
  ENDRELATION <: Relation[HYPERRELATION, END],
  END <: Node]
  (nodes: Traversable[raw.Node], relations: Traversable[raw.Relation],
   hyperRelationFactory: HyperRelationFactory[START, STARTRELATION, HYPERRELATION, ENDRELATION, END])
  : mutable.LinkedHashSet[HYPERRELATION] = {
    mutable.LinkedHashSet.empty ++ nodes.filter(_.labels.contains(hyperRelationFactory.label)).map { node =>
      val startRelation = relations.find(relation => relation.relationType == hyperRelationFactory.startRelationType && relation.endNode == node)
      val endRelation = relations.find(relation => relation.relationType == hyperRelationFactory.endRelationType && relation.startNode == node)
      // The Start- and EndRelation might not be part of the graph
      if(startRelation.isEmpty || endRelation.isEmpty)
        hyperRelationFactory.wrap(node)
      else
        hyperRelationFactory.wrap(startRelation.get, node, endRelation.get)
    }
  }
}

