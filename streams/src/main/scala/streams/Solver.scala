package streams

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = b.b1==goal && b.b2==goal

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   *
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   *
   * The function returns a stream of pairs: the first element of
   * each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   *
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    def appended = for {
      (block : Block, move : Move) <- b.legalNeighbors
    } yield (block, move :: history)
    appended.toStream
  }

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {
    def more = for {
      (b, l) <- neighbors
      if(!explored.contains(b))
    } yield (b, l)
    more
  }

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   *
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   *
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   *
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   *
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
    * Diese Eigenschaft ist bei uns gegeben, weil wir eine Breitensuche durchführen:
    * Sobald also ein Pfad gefunden wird, welcher die done(b) Bedingung erfüllt, wird dieser
    * eben auch als erster geyielded
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {

    val appended = initial match {
      case Stream.Empty => Stream.empty
      case (b,l) #:: xs => {
        val nbrs = newNeighborsOnly(neighborsWithHistory(b,l),explored+b)
        //(b,l) #:: nbrs
        nbrs ++ from(xs ++ nbrs, explored+b)
        //from(initial.tail,explored) ++ from(newNeighborsOnly(neighborsWithHistory(b,l),explored+b),explored+b)
      }
    }
    appended


    /*

    if(initial.isEmpty) Stream.empty
    else {
      val more : (Stream[(Block, List[Move])], Set[Block]) = for {
        (b,l) <- initial
        (next,exp) <- (newNeighborsOnly(neighborsWithHistory(b,l), explored+b), explored+b)
      } yield (next,exp)

      initial ++ from(more._1, (more._2 union explored))
    }
     */

  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((startBlock,List())), Set(startBlock))

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
    val finish = for {
      (b,l) <- pathsFromStart
      if(done(b))
    } yield (b,l)
    finish
  }

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = pathsToGoal.head._2.reverse
}
