package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._
import scala.annotation.tailrec

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("test times") {
    val result = times("nikitestmeniki".toCharArray.toList)

    def testCharCount(c: Char, count: Int) = c match {
      case 'n' => count == 2
      case 'i' => count == 4
      case 'k' => count == 2
      case 't' => count == 2
      case 'e' => count == 2
      case 'm' => count == 1
      case 's' => count == 1
    }

    @tailrec
    def iterate(list: List[(Char, Int)]): Boolean = list match {
      case Nil              => true
      case (c, count) :: xs => if (testCharCount(c, count)) iterate(xs) else false
    }

    assert(iterate(result))
  }

  test("test times empty list") {
    val result = times("".toCharArray.toList)

    def iterate(list: List[(Char, Int)]): Boolean = list match {
      case Nil              => true
      case (c, count) :: xs => false
    }

    assert(iterate(result))
  }

  test("test makeOrderedLeafList") {
    val data = times("nnnnbbbaah".toList)
    val result = makeOrderedLeafList(data)
    val expected = Leaf('h', 1) :: Leaf('a', 2) :: Leaf('b', 3) :: Leaf('n', 4) :: Nil
    assertResult(result)(expected)
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }
//
  test("create code tree") {
    val text = "aaaaaaaabbbcdefgh".toList
    val tree = createCodeTree(text)

    val encoded = encode(tree)("hn".toList)
    val decoded = decode(tree, encoded)

    Console println decodedSecret
    assert(true)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert") {
    new TestTrees {

      val text = "aaaaaaaabbbcdefgh".toList
      val tree = createCodeTree(text)

      val result = convert(tree)
      result.foreach { x =>
        val (char, bits) = x
        Console println s"CHAR: ${char} , BITS: ${bits}"
      }

      assert(true)
    }
  }
}
