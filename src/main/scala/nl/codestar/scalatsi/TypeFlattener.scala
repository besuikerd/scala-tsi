package nl.codestar.scalatsi

import nl.codestar.scalatsi.TypescriptType.{TypescriptAggregateType, TypescriptNamedType}

import scala.collection.mutable.ListBuffer

object TypeFlattener {

  type DependencyGraph = Map[TypescriptNamedType, Set[TypescriptNamedType]]
  def flatten(types: Seq[TypescriptNamedType]): Seq[TypescriptNamedType] = {
    val depencyGraph = buildDepencyGraph(types)
    stronglyConnectedComponents(depencyGraph).flatten
  }

  def buildDepencyGraph(types: Seq[TypescriptNamedType]): DependencyGraph =
    types.foldLeft[DependencyGraph](Map.empty)(buildDepencyGraph)

  def buildDepencyGraph(dependencyGraph: DependencyGraph, tp: TypescriptNamedType): DependencyGraph = {
    val dependencies = collectDependencies(tp)
    val nextGraph = dependencyGraph + (tp -> dependencies)
    dependencies.foldLeft(nextGraph)(buildDepencyGraph)
  }

  def collectDependencies(tp :TypescriptNamedType): Set[TypescriptNamedType] =
    tp match {
      case tp: TypescriptAggregateType =>
        val level = tp.nested.collect{case t: TypescriptNamedType => t}
        val children = level.flatMap(collectDependencies)
        level ++ children
      case _ => Set.empty
    }

  //tarjans strongly connected components
  def stronglyConnectedComponents(graph: DependencyGraph): Seq[Seq[TypescriptNamedType]] = {
    class Vector(var node: TypescriptNamedType, var index: Int, var lowLink: Int, var onStack: Boolean)

    val undefined = -1
    var index = 0
    val s = ListBuffer.empty[Vector]
    var sccs = Seq.empty[Seq[TypescriptNamedType]]
    val vectors = graph.keys.map(n => new Vector(n, undefined, undefined, false))
    val adjacencyList = vectors.map(v => v -> graph(v.node).map(dep => vectors.find(_.node == dep).get)).toMap

    vectors.foreach{v =>
      if(v.index == -1){
        strongConnect(v)
      }
    }

    def strongConnect(v: Vector): Unit ={
      v.index = index
      v.lowLink = index
      index = index + 1
      v +=: s
      v.onStack = true

      for(w <- adjacencyList(v)){
        if(w.index == undefined){
          strongConnect(w)
          v.lowLink = Math.min(v.lowLink, w.lowLink)
        } else if(w.onStack){
          v.lowLink = Math.min(v.lowLink, w.index)
        }
      }

      if(v.lowLink == v.index){
        var scc = Seq.empty[TypescriptNamedType]

        var w: Vector = null
        do {
          w = s.remove(0)
          w.onStack = false
          scc = scc ++ Seq(w.node)
        } while(w != v)
        sccs = sccs :+ scc
      }
    }

    sccs
  }
}
