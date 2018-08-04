package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i,h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min input") = forAll { (a: Int, b: Int) =>
    val minInput = if(a<b) a else b
    val h = insert(b, insert(a, empty))
    findMin(h) == minInput
  }

  property("check empty heap") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h) == true
  }

  property("check ordered delete list") = forAll { (h: H) =>
    def deleteH(h: H) : List[Int] = {
      if(isEmpty(h)) List()
      else {
        val minItem = findMin(h)
        minItem :: deleteH(deleteMin(h))
      }
    }
    def isSorted(l : List[Int]) : Boolean = {
      l == l.sorted
    }

    isSorted(deleteH(h)) == true
  }

  property("min of melded heaps") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val minimum = if(m1<m2) m1 else m2

    findMin(meld(h1,h2)) == minimum
  }

}
