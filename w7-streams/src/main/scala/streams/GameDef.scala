package streams

import common._

/**
 * This trait represents the layout and building blocks of the game
 */
trait GameDef {

  /**
   * The case class `Pos` encodes positions in the terrain.
   * 
   * IMPORTANT NOTE
   *  - The `x` coordinate denotes the position on the vertical axis
   *  - The `y` coordinate is used for the horizontal axis
   *  - The coordinates increase when moving down and right
   * 
   * Illustration:
   *
   *     0 1 2 3   <- y axis
   *   0 o o o o
   *   1 o o o o
   *   2 o # o o    # is at position Pos(2, 1)
   *   3 o o o o
   *  
   *   ^
   *   |
   *  
   *   x axis
   */
  case class Pos(x: Int, y: Int) {
    /** The position obtained by changing the `x` coordiante by `d` */
    def dx(d: Int) = copy(x = x + d, y)

    /** The position obtained by changing the `y` coordiante by `d` */
    def dy(d: Int) = copy(x, y = y + d)
  }

  /**
   * The position where the block is located initially.
   *
   * This value is left abstract, it will be defined in concrete
   * instances of the game.
   */
  val startPos: Pos

  /**
   * The target position where the block has to go.
   * This value is left abstract.
   */
  val goal: Pos

  /**
   * The terrain is represented as a function from positions to
   * booleans. The function returns `true` for every position that
   * is inside the terrain.
   *
   * As explained in the documentation of class `Pos`, the `x` axis
   * is the vertical one and increases from top to bottom.
   */
  type Terrain = Pos => Boolean

  
  /**
   * The terrain of this game. This value is left abstract.
   */
  val terrain: Terrain


  /**
   * In Bloxorz, we can move left, right, Up or down.
   * These moves are encoded as case objects.
   */
  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move

  /**
   * This function returns the block at the start position of
   * the game.
   */
  def startBlock: Block = Block(startPos, startPos)

  /**
   * A block is represented by the position of the two cubes that
   * it consists of. We make sure that `b1` is lexicographically
   * smaller than `b2`.
   */
  case class Block(b1: Pos, b2: Pos) {

    // checks the requirement mentioned above
    require(b1.x <= b2.x && b1.y <= b2.y, "Invalid block position: b1=" + b1 + ", b2=" + b2)

    /**
     * Returns a block where the `x` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def dx(d1: Int, d2: Int) = Block(b1.dx(d1), b2.dx(d2))

    /**
     * Returns a block where the `y` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def dy(d1: Int, d2: Int) = Block(b1.dy(d1), b2.dy(d2))


    /** The block obtained by moving left */
    def left = if (isStanding)         dy(-2, -1)
               else if (b1.x == b2.x)  dy(-1, -2)
               else                    dy(-1, -1)

    /** The block obtained by moving right */
    def right = if (isStanding)        dy(1, 2)
                else if (b1.x == b2.x) dy(2, 1)
                else                   dy(1, 1)

    /** The block obtained by moving up */
    def up = if (isStanding)           dx(-2, -1)
             else if (b1.x == b2.x)    dx(-1, -1)
             else                      dx(-1, -2)

    /** The block obtained by moving down */
    def down = if (isStanding)         dx(1, 2)
               else if (b1.x == b2.x)  dx(1, 1)
               else                    dx(2, 1)


    /**
     * Returns the list of blocks that can be obtained by moving
     * the current block, together with the corresponding move.
     */
    def neighbors: List[(Block, Move)] =
      List( (left, Left), (right, Right), (up, Up), (down, Down) )        

    /**
     * Returns the list of positions reachable from the current block
     * which are inside the terrain.
     */
    def legalNeighbors: List[(Block, Move)] = neighbors filter {
      case (block, _) => block.isLegal
    }

    /**
     * Returns `true` if the block is standing.
     */
    def isStanding: Boolean = b1 == b2

    /**
     * Returns `true` if the block is entirely inside the terrain.
     */
    def isLegal: Boolean = terrain(b1) && terrain(b2)

  }
}
