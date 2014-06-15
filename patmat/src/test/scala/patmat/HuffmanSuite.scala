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

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
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
  
  test("times(\"hello, world\")") {
    val list = times(string2Chars("hello, world"))
    assert(list.contains(('h', 1)))
    assert(list.contains(('e', 1)))
    assert(list.contains(('l', 3)))
    assert(list.contains(('o', 2)))
    assert(list.contains((',', 1)))
    assert(list.contains((' ', 1)))
    assert(list.contains(('w', 1)))
    assert(list.contains(('r', 1)))
    assert(list.contains(('d', 1)))
    assert(list.length == 9)   
  }
  
  test("makeOrderedLeafList(\"hello, world\")") {
    val leafList = makeOrderedLeafList(times(string2Chars("hello, world")))
    assert(leafList(8) === Leaf('l', 3))
    assert(leafList(7) === Leaf('o', 2))
    assert(leafList.length == 9)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("encode a very short text"){  
    new TestTrees {
      assert(encode(t1)("ab".toList) ===  List(0,1))
    }
  }
  test("decodedSecret") {
      assert(Huffman.decodedSecret === string2Chars("huffmanestcool"))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
