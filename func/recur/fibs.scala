object Solution {
    
    def fibonacci(x:Int):Int = {
        def loop (x: Int, y: Int, n: Int): Int = n match {
            case 0 => x + y
            case i => loop(y, x + y, i - 1)
        }
        x match {
         case 1 => 0
         case 2 => 1
         case _ => loop(0, 1, x - 3)
        }
    }

    def main(args: Array[String]) {
         /** This will handle the input and output**/
         println(fibonacci(readInt()))

    }
}
