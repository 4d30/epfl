package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:

  lazy val genHeap: Gen[H] = 
    for 
      x <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    yield
      insert(x, h)

  given Arbitrary[H] = Arbitrary(genHeap)

// for any heap, adding the minimal element, and then finding it, should return the element in question
  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
// adding a single element to an empty heap, and then removing this element, should yield the element in question 
  property("min1") = forAll { (a: A) =>
    val h = insert(a, empty)
    findMin(h) == a
  }
// If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back. 
  property("min2") = forAll { (a: A, b: A) =>
    val t = if (a < b) then a else b
    val h = insert(b, insert(a, empty))
    findMin(h) == t
  }

// If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty
  property("delSingleton") = forAll { (a: A) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }
  
// Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  property("sortedSeq") = forAll{ (h: H) =>
    def popMin(heap: H, acc: List[A]): List[A] = isEmpty(heap) match {
      case true => acc.reverse
      case false => popMin(deleteMin(heap), findMin(heap) :: acc)}
    val xs = popMin(h, Nil)
    xs == xs.sorted
  }


// Finding a minimum of the melding of any two heaps should return a minimum of one or the other
  property("meldMin") = forAll{ (ha: H, hb: H) =>
    (findMin(ha) < findMin(hb)) match
      case true => findMin(meld(ha,hb)) == findMin(ha)
      case false => findMin(meld(ha,hb)) == findMin(hb)
  }


  property("meldMinMo") = forAll { (h1: H, h2: H) =>
     def remoMin(ts: H, as: List[A]): List[A] = {
       if (isEmpty(ts)) as
       else findMin(ts) :: remoMin(deleteMin(ts), as)
     }
     remoMin(meld(h1, h2), Nil) == remoMin(meld(deleteMin(h1), insert(findMin(h1), h2)), Nil)


  }
