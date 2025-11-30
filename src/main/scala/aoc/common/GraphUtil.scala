package aoc.common

import scala.collection.mutable

object GraphUtil:

  def shortestPath[Vertex, Edge, Cost: Numeric](
    from: Vertex,
    reachedTo: Vertex => Boolean,
    neighborFn: Vertex => Seq[Edge],
    edgeDestinationFn: Edge => Vertex,
    costFn: Edge => Cost
  )(implicit
    N: Numeric[Cost]
  ): Option[(List[Vertex], Cost)] =
    if reachedTo(from) then return Some(Nil, N.zero)
    var previousVertex: Map[Vertex, Vertex]      = Map()
    case class PathEnding(prev: Vertex, end: Vertex, pathCost: Cost)
    implicit val ord: Ordering[PathEnding]       = (x: PathEnding, y: PathEnding) => N.compare(y.pathCost, x.pathCost)
    val queue: mutable.PriorityQueue[PathEnding] =
      scala.collection.mutable.PriorityQueue[PathEnding](
        neighborFn(from)
          .map(e => PathEnding(from, edgeDestinationFn(e), costFn(e)))*
      )

    var foundTo  = Option.empty[Vertex]
    var distance = N.zero
    while queue.nonEmpty && foundTo.isEmpty do
      val pathEnd = queue.dequeue()
      if !previousVertex.contains(pathEnd.end) then
        // Found shortest path to this vertex
        previousVertex = previousVertex + (pathEnd.end -> pathEnd.prev)
        if reachedTo(pathEnd.end) then
          foundTo = Some(pathEnd.end)
          distance = pathEnd.pathCost
        else
          queue.addAll(
            neighborFn(pathEnd.end).map(e =>
              PathEnding(pathEnd.end, edgeDestinationFn(e), N.plus(costFn(e), pathEnd.pathCost))
            )
          )

    foundTo.map(to =>
      // Extract path
      (LazyList.iterate(to)(previousVertex).takeWhile(_ != from).reverse.prepended(from).toList, distance)
    )
  end shortestPath

  def shortestPathToAll[Vertex, Edge, Cost: Numeric](
    from: Vertex,
    neighborFn: Vertex => Seq[Edge],
    edgeDestinationFn: Edge => Vertex,
    costFn: Edge => Cost
  )(implicit
    N: Numeric[Cost]
  ): (Map[Vertex, Cost], Map[Vertex, Vertex]) =
    var previousVertex: Map[Vertex, Vertex]      = Map()
    var shortestCostTo: Map[Vertex, Cost]        = Map(from -> N.zero)
    case class PathEnding(prev: Vertex, end: Vertex, pathCost: Cost)
    implicit val ord: Ordering[PathEnding]       = (x: PathEnding, y: PathEnding) => N.compare(y.pathCost, x.pathCost)
    val queue: mutable.PriorityQueue[PathEnding] =
      scala.collection.mutable.PriorityQueue[PathEnding](
        neighborFn(from)
          .map(e => PathEnding(from, edgeDestinationFn(e), costFn(e)))*
      )

    while queue.nonEmpty do
      val pathEnd = queue.dequeue()
      if !previousVertex.contains(pathEnd.end) then
        // Found shortest path to this vertex
        shortestCostTo = shortestCostTo + (pathEnd.end -> pathEnd.pathCost)
        previousVertex = previousVertex + (pathEnd.end -> pathEnd.prev)
        queue.addAll(
          neighborFn(pathEnd.end).map(e =>
            PathEnding(pathEnd.end, edgeDestinationFn(e), N.plus(costFn(e), pathEnd.pathCost))
          )
        )

    (shortestCostTo, previousVertex)
  end shortestPathToAll

  // This finds all shortest paths as there could be many with same cost.
  // Returns map with previous vertices for each vertex in a shortest path.
  def allShortestPaths[Vertex, Edge, Cost: Numeric](
    from: Vertex,
    reachedTo: Vertex => Boolean,
    neighborFn: Vertex => Seq[Edge],
    edgeDestinationFn: Edge => Vertex,
    costFn: Edge => Cost
  )(using
    N: Numeric[Cost],
    O: Ordering[Cost]
  ): Option[(Map[Vertex, List[Vertex]], Cost)] =
    if reachedTo(from) then return Some(Map.empty, N.zero)
    var previousVertices: Map[Vertex, List[Vertex]] = Map.empty
    var shortestCostTo: Map[Vertex, Cost]           = Map(from -> N.zero)
    case class PathEnding(prev: Vertex, end: Vertex, pathCost: Cost)
    implicit val ord: Ordering[PathEnding]          = (x: PathEnding, y: PathEnding) => N.compare(y.pathCost, x.pathCost)
    val queue: mutable.PriorityQueue[PathEnding]    =
      scala.collection.mutable.PriorityQueue[PathEnding](
        neighborFn(from)
          .map(e => PathEnding(from, edgeDestinationFn(e), costFn(e)))*
      )

    var foundTo  = Option.empty[PathEnding]
    var done     = false
    var distance = N.zero
    while queue.nonEmpty && !done do
      val pathEnd = queue.dequeue()
      if foundTo.exists(f => O.lt(f.pathCost, pathEnd.pathCost))
      then done = true
      else if pathEnd.end != from then
        previousVertices.get(pathEnd.end) match
          case None                                                          =>
            // Found first shortest path to this vertex
            previousVertices = previousVertices + (pathEnd.end -> (pathEnd.prev :: Nil))
            if reachedTo(pathEnd.end) then
              foundTo = Some(pathEnd)
              distance = pathEnd.pathCost

            shortestCostTo = shortestCostTo + (pathEnd.end -> pathEnd.pathCost)
            queue.addAll(
              neighborFn(pathEnd.end).map(e =>
                PathEnding(pathEnd.end, edgeDestinationFn(e), N.plus(costFn(e), pathEnd.pathCost))
              )
            )
          case Some(prev) if shortestCostTo(pathEnd.end) == pathEnd.pathCost =>
            // Found another shortest path to this vertex
            previousVertices = previousVertices.updatedWith(pathEnd.end)(_ => Some(pathEnd.prev :: prev))
          case _                                                             =>
            // Not shortest path.
            ()
      end if
    end while

    foundTo.map(_ => previousVertices -> distance)
  end allShortestPaths

end GraphUtil
