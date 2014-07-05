package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = {
		b.isStanding && b.b1.x == goal.x && b.b2.y == goal.y
	}

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
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   * 
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
		val vNeighbors = b.legalNeighbors
		vNeighbors.map(x => (x._1, x._2::history)).toStream
	}

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {

		neighbors.filter(ele =>{
			val block  = ele._1
			!explored(block)
		})
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
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
			if (initial.length == 0) {
				Stream()
			} else {
				val head = initial.head
				val neighborMoves = head._1.legalNeighbors.filter(x => {!explored(x._1)}).map(x=>{(x._1, x._2::head._2)})
				val leftToExplore = initial.tail #::: neighborMoves.toStream
				val newExplored = explored ++ neighborMoves.map(x => x._1)
				head #:: from(leftToExplore, newExplored)
			}
	}

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = {
		val s = Stream((startBlock, List()))
		val explored = Set(startBlock)
		from(s, explored)
	}

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {Stream()}

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
	def getSolution(paths:Stream[(Block,List[Move])], goalBlock:Block):List[Move] = {
		println("gettting solution " + paths.length)
		val headOption = paths.headOption
		headOption match {
			case Some(head) => {
				if (head._1 == goalBlock) head._2.reverse else getSolution(paths.tail, goalBlock)
			}
			case None  => {
				List()
			}
		}
	}
  lazy val solution: List[Move] = {
		val goalBlock = new Block(goal, goal);
		/*
	  for (x <- pathsFromStart) {
			if (x._1 == goalBlock) {
				return x._2.reverse
			}
		}
		List()
		*/
		val op = pathsFromStart.find(x => x._1 == goalBlock)
		op match {
			case Some(x) => x._2.reverse
			case None => List()
		}

	}
}
