import scala.util.Random

object MazeGen {
    type Pos = (Int, Int)

    case class Maze(maze: Array[Array[Boolean]]) {

        def size = maze.length

        override def toString = {
            maze map(_ map {if (_) "[]" else "  "} mkString "") mkString "\n"
        }

        def apply(p: Pos) = {
        	if (   p._1 <= 0
        		|| p._1 >= size - 1
        		|| p._2 <= 0
        		|| p._2 >= size - 1)
        		false
        	else
	        	maze(p._2)(p._1)
        }
        
        def update(p: Pos, v: Boolean) = maze(p._2)(p._1) = v
    }
    
    def up(p: Pos): Pos = (p._1, p._2 + 1)
    def down(p: Pos): Pos = (p._1, p._2 - 1)
    def left(p: Pos): Pos = (p._1 - 1, p._2)
    def right(p: Pos): Pos = (p._1 + 1, p._2)
	  
	def main(args: Array[String]) {
		val input = try { args(0).toInt }
		catch {
			case e: Exception => 
				println("Please provide an odd number >= 3 as first parameter")
				return
		}
		
		val size = if (input < 3) 3 else {
			// makes a nicer looking maze
			if (input % 2 == 0) {
				println(s"Need an odd number, using size = ${input + 1}")
				input + 1
			}
			else input
		}
        
        val maze = Maze(Array.tabulate(size, size)((_,_) => true))

		val r = new Random

	    def doubleStep(p: Pos, f: Pos => Pos): Boolean = {
    		if (maze(f(p)) && maze(f(f(p)))) {
    			maze(f(p)) = false
    			maze(f(f(p))) = false
    			true
    		}
    		else
    			false
    	}

		val possibleMoves = List(up(_: Pos), down(_: Pos), left(_: Pos), right(_: Pos))
	    def computePath(p: Pos) {
	    	for (direction <- Random.shuffle(possibleMoves)) {
	    			if (doubleStep(p, direction))
	    				computePath(direction(direction(p)))
	    	}
	    }
	
		val s = r.nextInt(size - 2) + 1
		val sOdd = if (s % 2 == 0) s + 1 else s	// makes a nicer looking maze
		val start = (sOdd, 1)
		val exit  = (r.nextInt(size - 2) + 1, size - 1)
		maze(start) = false
		maze(down(start)) = false // mark "entrance"
		maze(exit) = false        // mark "exit"
		
		computePath(start)

        println(maze)
    }

}
