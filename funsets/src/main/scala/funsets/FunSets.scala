package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
    * The following is a so called "type alias". Wherever this type occurs
    * it will get replaced by this definition here.
    * Note: This type alias is actually a function. Types of this member can hence be called (have a look at contains)!
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
    * Returns the set with only one member
   */
  def singletonSet(elem: Int): Set = {
    def single(i: Int): Boolean = if(i==elem) true else false
    single
  }
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = {
      def uniF(i : Int) : Boolean = {
        if(contains(t,i) || contains(s,i)) true else false
      }
      uniF
    }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = {
      def intF(i : Int) : Boolean = {
        if(contains(t,i) && contains(s,i)) true else false
      }
      intF
    }
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = {
      def diffF(i : Int) : Boolean = {
        if(!contains(t,i) && contains(s,i)) true else false
      }
      diffF

    }
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = {
      // das prädikat p ist im Grunde auch nur ein Set...
      intersect(s,p)
    }
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a==bound) contains(s,a) && p(a)
      else if ((contains(s,a) && p(a)) || !contains(s,a)) iter(a+1)
      else false
    }
    iter(-bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = ???
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = ???
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
