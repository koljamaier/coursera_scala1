package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("()".toList))
    println(balance("())(".toList))
    println(balance(":-)".toList))
    println(balance("( ( ())()".toList))
    println(balance("for: (if (zero? x) max (/ 1 x))".toList))

    println(countChange(4, List(1, 2)))

    println("test")

  }

  /**
    * Exercise 1
    *
    * 1
    * 1 1
    * 1 2 1
    * 1 3 3 1
    * 1 4 6 4 1
    **
    *Write a function that computes the elements of Pascal’s triangle by means of a recursive process.
    * Column c, row r.
    * pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || r == 1 || c == 0 || c == r) return 1
    else if (c - 1 < 0)
      return pascal(c, r)
    else
      return pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    * true: for: (if (zero? x) max (/ 1 x))
    * true: I told him (that it’s not (yet) done). (But he wasn’t listening)
    *
    * false: :-)
    * false: ())(
    *
    * ( ())
    * ()
    */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true
    else if (chars.head == ')') false
    else if (chars.head == '(') {
      val closed: Int = chars.mkString.indexOf(")")
      if (closed > -1)
        balance(chars.tail.zipWithIndex.filter(_._2 != (closed - 1)).map(_._1))
      else false
    }
    else balance(chars.tail)
  }

  /**
    * Exercise 3
    * Write a recursive function that counts how many different ways you can make change for an amount,
    * given a list of coin denominations.
    * For example, there are 3 ways to give change for 4 if you have coins with denomination
    * 1 and 2: 1+1+1+1, 1+1+2, 2+2
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    //return money%coins.head + countChange(money,coins.tail)
    if (money == 0) 1
    else if (coins.isEmpty || money < 0)
      return 0
    else if (money % coins.head == money) // wenn  money=7 und coins.head=9 z.B. -> 7%9=7
      countChange(money, coins.tail)
    else if (coins.head == money) // coins.head kann getrost weggeworfen werden -> 7==7
      1 + countChange(money, coins.tail)
    // die nächsten beiden Fälle sind wichtig. Hier dürfen wir coins.head nicht wegschmeißen
    //else if(money%coins.head==0) // z.B. 7 % 1 -> +1 für die Kombination, dass money nur durch coins.head dargestellt wird
    //  countChange(money, coins.tail) + countChange(money-coins.head, coins)
    //else if(0<money%coins.head && money%coins.head<money) // wenn es dazwischen liegt gibt Kombinationen zurück
    //  countChange(money%coins.head,coins.tail) + countChange(money,coins.tail)
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)

  }
}
