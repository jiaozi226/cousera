package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
		println(balance("())(".toList));
		println(balance(":-)".toList)); println(balance("(if (zero? x) max (/ 1 x))".toList));
		println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList));
		println(countChange(4, List(1,2)));
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
		if (c == 0 || c == r) {
			1
		} else if (c > r) {
			0
		} else {
			pascal(c, r-1) + pascal(c -1, r -1);
		}
	}

  def innerBalance(unmatched:Int, chars: List[Char]):Boolean = {
		if (unmatched < 0) {
			false
		} else {
			chars match {
				case List() => unmatched == 0
				case head::tail  => {
					if (head == '(')
						innerBalance(unmatched + 1, tail)
					else if (head == ')') 
						innerBalance(unmatched - 1, tail) else innerBalance(unmatched, tail); } }
		}
	}
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
		innerBalance(0, chars);
	}

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
		if (money == 0) {
			1
		} else if (coins.length == 0) {
			0
		} else {
			val h = coins.head;
			val t = coins.tail;
			val cnt = money / h;
			val res = new Array[Int](cnt + 1);
			for (i <- 0 to cnt) {
				res(i) =  countChange(money - i*h, t);
			}
			res.reduce(_ + _);
		}
	}
}
