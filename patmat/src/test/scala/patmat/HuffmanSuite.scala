package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of trees") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("Times function") {
    val freqList : List[(Char, Int)] = times("helloworld".toList)

    assert(freqList.size === 7)

    freqList.foreach(validateCount(_))
    def validateCount(pair : (Char, Int)) = pair match {
      case ('l', freq) => assert(freq === 3)
      case ('o', freq) => assert(freq === 2)
      case ('h', freq) => assert(freq === 1)
      case (_, _) => assert(true) // match other patterns - default case
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until function") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'), 7))
  }

  test("decode function") {
    new TestTrees() {
    	assert(decode(t1, List(0)) === List('a'))
    	assert(decode(t1, List(1,0)) === List('b', 'a'))
    	assert(decode(t2, List(0,1,0,1,1)) === List('b', 'b', 'd'))
    }
    assert(decode(Leaf('f', 3), List()) === List('f'))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("Quick encode function") {
    new TestTrees {
      assert(quickEncode(t1)("bba".toList) === List(1, 1, 0))
      assert(quickEncode(t2)("bbda".toList) === List(0, 1, 0, 1, 1, 0, 0))
    }
  }
}
