/*
 * Copyright (c) 2014 Alexandre Archambault
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless.refinedstd

import shapeless._
import shapeless.refinedstd.syntax._
import shapeless.test.illTyped
import org.scalatest._
import shapeless.refinedstd.test._

class IteratorTupleExtensionsSpec extends FlatSpec with Matchers {

  "An iterator" should "return HNil when asked its first 0 values" in {
    def it = Iterator.range(0, 5)
    def firstZeroOption = it.takeT(0)

    equalInferredTypeAndValue(Option(()), firstZeroOption) shouldBe true
  }

  it should "return its first five values when asked to do so" in {
    def it = Iterator.range(0, 10)
    def firstFiveOption = it.takeT(5)

    equalInferredTypeAndValue(Option((0, 1, 2, 3, 4)), firstFiveOption) shouldBe true
  }

  it should "return None when too much elements are taken from it" in {
    def it = Iterator.range(0, 5)
    def firstSixOption = it.takeT(6)

    val rep6 = util.TupleRepeat(6)
    equalInferredTypeAndValue(Option.empty[rep6.Out[Int]], firstSixOption) shouldBe true
  }

  it should "return HNil when asked a slice from n to n" in {
    def it = Iterator.range(0, 5)
    def sliceOption = it.sliceT(2, 2)

    equalInferredTypeAndValue(Option(()), sliceOption) shouldBe true
  }

  it should "not compile when asked a slice from n to n - 1" in {
    def it = Iterator.range(0, 5)
    illTyped(""" it.sliceT(2, 1) """)
  }

  it should "return the right slice when asked to do so" in {
    def it = Iterator.range(0, 10)
    def sliceOption = it.sliceT(2, 6)

    equalInferredTypeAndValue(Option((2, 3, 4, 5)), sliceOption) shouldBe true
  }

  it should "take 3 to the right" in {
    def it = Iterator.range(0, 7)
    def res = it.takeRightT(3)

    equalInferredTypeAndValue(res, Option((4, 5, 6))) shouldBe true
  }

  it should "take nothing to the right" in {
    def it = Iterator.range(0, 3)
    def res = it.takeRightT(4)

    val helper = util.TupleRepeat(4)
    equalInferredTypeAndValue(res, Option.empty[helper.Out[Int]]) shouldBe true
  }

  it should "scan 3 from the left" in {
    def it = Iterator.range(0, 7)
    def res = it.scanLeftT(3, 0)(_ + _)

    equalInferredTypeAndValue(res, Option((0, 0, 1, 3))) shouldBe true
  }

  it should "scan nothing from the left" in {
    def it = Iterator.range(0, 3)
    def res = it.scanLeftT(4, 0)(_ + _)

    val helper = util.TupleRepeat(5)
    equalInferredTypeAndValue(res, Option.empty[helper.Out[Int]]) shouldBe true
  }

  it should "scan 3 from the right" in {
    def it = Iterator.range(0, 7)
    def res = it.scanRightT(3, 0)(_ + _)

    equalInferredTypeAndValue(res, Option((15, 11, 6, 0))) shouldBe true
  }

  it should "scan nothing from the right" in {
    def it = Iterator.range(0, 3)
    def res = it.scanRightT(4, 0)(_ + _)

    val helper = util.TupleRepeat(5)
    equalInferredTypeAndValue(res, Option.empty[helper.Out[Int]]) shouldBe true
  }

  it should "find an empty list of indices" in {
    def it = Iterator.range(0, 7)
    def res = it.indicesWhereT(0) { _ > 3 }

    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }

  it should "find no indices" in {
    def it = Iterator.range(0, 7)
    def res = it.indicesWhereT(2) { _ >= 7 }

    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find three indices" in {
    def it = Iterator.range(0, 7)
    def res = it.indicesWhereT(3) { _ % 3 == 0 }

    equalInferredTypeAndValue(res, Option((0, 3, 6))) shouldBe true
  }

  it should "find an empty list of indices of" in {
    def it = Iterator.range(0, 3) ++ Iterator.fill(3)(1)
    def res = it.indicesOfT(0, 1)

    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }

  it should "find no indices of" in {
    def it = Iterator.range(0, 3) ++ Iterator.fill(3)(1)
    def res = it.indicesOfT(2, 3)

    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find three indices of" in {
    def it = Iterator.range(0, 3) ++ Iterator.fill(3)(1)
    def res = it.indicesOfT(3, 1)

    equalInferredTypeAndValue(res, Option((1, 3, 4))) shouldBe true
  }

  it should "refuse to be grouped by 0" in {
    def it = Iterator.range(0, 5)

    illTyped(""" it.groupedT(0) """)
  }

  it should "be grouped by 1" in {
    def it = Iterator.range(0, 5)
    def other = Iterator(Tuple1(0), Tuple1(1), Tuple1(2), Tuple1(3), Tuple1(4))

    equalInferredType(it.groupedT(1), other)

    (it.groupedT(1) sameElements other) shouldBe true
  }

  it should "be grouped by 5" in {
    def it = Iterator.range(0, 10)
    def other = Iterator((0, 1, 2, 3, 4), (5, 6, 7, 8, 9))

    equalInferredType(it.groupedT(5), other)

    (it.groupedT(5) sameElements other) shouldBe true
  }

  it should "refuse to slide by 0" in {
    def it = Iterator.range(0, 5)

    illTyped(""" it.slidingT(0) """)
    illTyped(""" it.slidingT(2, 0) """)
  }

  it should "be sliding by 2" in {
    def it = Iterator.range(0, 5)
    def other = Iterator((0, 1), (1, 2), (2, 3), (3, 4))

    equalInferredType(it.slidingT(2), other)

    (it.slidingT(2) sameElements other) shouldBe true
  }

  it should "be sliding by 3, stepping by 2" in {
    def it = Iterator.range(0, 6)
    def other = Iterator((0, 1, 2), (2, 3, 4))

    equalInferredType(it.slidingT(3, 2), other)

    (it.slidingT(3, 2) sameElements other) shouldBe true
  }
  
  it should "duplicate 0-times" in {
    def it = Iterator.range(0, 6)
    equalInferredType(it.duplicatesT(0), ())
  }

  it should "duplicate it-self" in {
    def it = Iterator.range(0, 6)
    
    equalInferredType(it.duplicatesT(1), Tuple1(it))

    (it.duplicatesT(1)._1 sameElements it) shouldBe true
  }

  it should "duplicate it-self 3 times" in {
    def it = Iterator.range(0, 6)

    equalInferredType(it.duplicatesT(3), (it, it, it))

    val (it1, it2, it3) = it.duplicatesT(3)
    (it1 sameElements it) shouldBe true
    (it2 sameElements it) shouldBe true
    (it3 sameElements it) shouldBe true
  }

  it should "zip itself" in {
    def it = Iterator.range(0, 10)
    def tlIt = Iterator.range(0, 10).map(Tuple1(_))

    equalInferredType(it.zipN(()), tlIt)

    (it.zipN(()) sameElements tlIt) shouldBe true
  }

  it should "zip with another" in {
    def it = Iterator.range(0, 10)
    def it2 = Iterator.range(10, 0, -1)
    def tlIt = Iterator.range(0, 10).map(i => (i, 10 - i))

    equalInferredType(it zipN Tuple1(it2), tlIt)

    (it zipN Tuple1(it2) sameElements tlIt) shouldBe true
  }

  it should "zip with 3 other" in {
    def it = Iterator.range(0, 10)
    def it1 = Iterator.range(1, 11)
    def it2 = Iterator.range(2, 12)
    def it3 = Iterator.range(3, 13)
    def tlIt = Iterator.range(0, 10).map(i => (i, i+1, i+2, i+3))

    equalInferredType(it zipN (it1, it2, it3), tlIt)

    (it zipN (it1, it2, it3) sameElements tlIt) shouldBe true
  }

  it should "zip one element" in {
    def it = Iterator.range(0, 10)
    def tlIt = Iterator.range(0, 10).map(Tuple1(_))

    equalInferredType(Iterator.zip(Tuple1(it)), tlIt)

    (Iterator.zip(Tuple1(it)) sameElements tlIt) shouldBe true
  }

  it should "zip 3 elements" in {
    def it0 = Iterator.range(0, 10)
    def it1 = Iterator.range(1, 11)
    def it2 = Iterator.range(2, 12)
    def tlIt = Iterator.range(0, 10).map(i => (i, i+1, i+2))

    equalInferredType(Iterator.zip(it0, it1, it2), tlIt)

    (Iterator.zip(it0, it1, it2) sameElements tlIt) shouldBe true
  }

}

class IteratorHListExtensionsSpec extends FlatSpec with Matchers {

  "An iterator" should "return HNil when asked its first 0 values" in {
    def it = Iterator.range(0, 5)
    val firstZeroOption = it.takeH(0)

    equalInferredTypeAndValue(Option(HNil: HNil), firstZeroOption) shouldBe true
  }

  it should "return its first five values when asked to do so" in {
    def it = Iterator.range(0, 10)
    val firstFiveOption = it.takeH(5)

    equalInferredTypeAndValue(Option(HList(0, 1, 2, 3, 4)), firstFiveOption) shouldBe true
  }

  it should "return None when too much elements are taken from it" in {
    def it = Iterator.range(0, 5)
    val firstSixOption = it.takeH(6)

    val rep6 = util.HListRepeat(6)
    equalInferredTypeAndValue(Option.empty[rep6.Out[Int]], firstSixOption) shouldBe true
  }

  it should "return HNil when asked a slice from n to n" in {
    def it = Iterator.range(0, 5)
    val sliceOption = it.sliceH(2, 2)

    equalInferredTypeAndValue(Option(HNil: HNil), sliceOption) shouldBe true
  }

  it should "not compile when asked a slice from n to n - 1" in {
    def it = Iterator.range(0, 5)
    illTyped(""" it.sliceH(2, 1) """)
  }

  it should "return the right slice when asked to do so" in {
    def it = Iterator.range(0, 10)
    val sliceOption = it.sliceH(2, 6)

    equalInferredTypeAndValue(Option(HList(2, 3, 4, 5)), sliceOption) shouldBe true
  }

  it should "take 3 to the right" in {
    def it = Iterator.range(0, 7)
    val res = it.takeRightH(3)

    equalInferredTypeAndValue(res, Option(HList(4, 5, 6))) shouldBe true
  }

  it should "take nothing to the right" in {
    def it = Iterator.range(0, 3)
    val res = it.takeRightH(4)

    val helper = util.HListRepeat(4)
    equalInferredTypeAndValue(res, Option.empty[helper.Out[Int]]) shouldBe true
  }

  it should "scan 3 from the left" in {
    def it = Iterator.range(0, 7)
    val res = it.scanLeftH(0, 3)(_ + _)

    equalInferredTypeAndValue(res, Option(HList(0, 0, 1, 3))) shouldBe true
  }

  it should "scan nothing from the left" in {
    def it = Iterator.range(0, 3)
    val res = it.scanLeftH(0, 4)(_ + _)

    val helper = util.HListRepeat(5)
    equalInferredTypeAndValue(res, Option.empty[helper.Out[Int]]) shouldBe true
  }

  it should "scan 3 from the right" in {
    def it = Iterator.range(0, 7)
    val res = it.scanRightH(0, 3)(_ + _)

    equalInferredTypeAndValue(res, Option(HList(15, 11, 6, 0))) shouldBe true
  }

  it should "scan nothing from the right" in {
    def it = Iterator.range(0, 3)
    val res = it.scanRightH(0, 4)(_ + _)

    val helper = util.HListRepeat(5)
    equalInferredTypeAndValue(res, Option.empty[helper.Out[Int]]) shouldBe true
  }

  it should "be sliding by 2 stepping by 1" in {
    def it = Iterator.range(0, 5)
    def expected = Iterator(HList(0, 1), HList(1, 2), HList(2, 3), HList(3, 4))

    def res = it.slidingH(2, 1)

    equalInferredType(res, expected)
    (res sameElements expected) shouldBe true
  }

  it should "be sliding by 2 stepping by 1 by default" in {
    def it = Iterator.range(0, 5)
    def expected = Iterator(HList(0, 1), HList(1, 2), HList(2, 3), HList(3, 4))

    def res = it.slidingH(2)

    equalInferredType(res, expected)
    (res sameElements expected) shouldBe true
  }

  it should "be sliding by 3 stepping by 2" in {
    def it = Iterator.range(0, 6)
    def expected = Iterator(HList(0, 1, 2), HList(2, 3, 4))

    def res = it.slidingH(3, 2)

    equalInferredType(res, expected)
    (res sameElements expected) shouldBe true
  }

  it should "find an empty list of indices" in {
    def it = Iterator.range(0, 7)
    def res = it.indicesWhereH(0) { _ > 3 }

    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no indices" in {
    def it = Iterator.range(0, 7)
    def res = it.indicesWhereH(2) { _ >= 7 }

    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find three indices" in {
    def it = Iterator.range(0, 7)
    def res = it.indicesWhereH(3) { _ % 3 == 0 }

    equalInferredTypeAndValue(res, Option(HList(0, 3, 6))) shouldBe true
  }

  it should "find an empty list of indices of" in {
    def it = Iterator.range(0, 3) ++ Iterator.fill(3)(1)
    def res = it.indicesOfH(0, 1)

    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no indices of" in {
    def it = Iterator.range(0, 3) ++ Iterator.fill(3)(1)
    def res = it.indicesOfH(2, 3)

    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find three indices of" in {
    def it = Iterator.range(0, 3) ++ Iterator.fill(3)(1)
    def res = it.indicesOfH(3, 1)

    equalInferredTypeAndValue(res, Option(HList(1, 3, 4))) shouldBe true
  }

  it should "refuse to be grouped by 0" in {
    def it = Iterator.range(0, 5)

    illTyped(""" it.groupedH(0) """)
  }

  it should "be grouped by 1" in {
    def it = Iterator.range(0, 5)
    def other = Iterator(0 :: HNil, 1 :: HNil, 2 :: HNil, 3 :: HNil, 4 :: HNil)

    equalInferredType(it.groupedH(1), other)

    (it.groupedH(1) sameElements other) shouldBe true
  }

  it should "be grouped by 5" in {
    def it = Iterator.range(0, 10)
    def other = Iterator(0 :: 1 :: 2 :: 3 :: 4 :: HNil, 5 :: 6 :: 7 :: 8 :: 9 :: HNil)

    equalInferredType(it.groupedH(5), other)

    (it.groupedH(5) sameElements other) shouldBe true
  }

  it should "duplicate 0-times" in {
    def it = Iterator.range(0, 6)
    equalInferredType(it.duplicatesH(0), HNil: HNil)
  }

  it should "duplicate it-self" in {
    def it = Iterator.range(0, 6)

    equalInferredType(it.duplicatesH(1), HList(it))

    (it.duplicatesH(1).head sameElements it) shouldBe true
  }

  it should "duplicate it-self 3 times" in {
    def it = Iterator.range(0, 6)

    equalInferredType(it.duplicatesH(3), HList(it, it, it))

    val (it1 :: it2 :: it3 :: HNil) = it.duplicatesH(3)
    (it1 sameElements it) shouldBe true
    (it2 sameElements it) shouldBe true
    (it3 sameElements it) shouldBe true
  }

  it should "zip itself" in {
    def it = Iterator.range(0, 10)
    def hlIt = Iterator.range(0, 10).map(_ :: HNil)

    equalInferredType(it zipN HNil, hlIt)

    (it zipN HNil sameElements hlIt) shouldBe true
  }

  it should "zip with another" in {
    def it = Iterator.range(0, 10)
    def it2 = Iterator.range(10, 0, -1)
    def tlIt = Iterator.range(0, 10).map(i => i :: (10 - i) :: HNil)

    equalInferredType(it zipN (it2 :: HNil), tlIt)

    (it zipN (it2 :: HNil) sameElements tlIt) shouldBe true
  }

  it should "zip with 3 other" in {
    def it = Iterator.range(0, 10)
    def it1 = Iterator.range(1, 11)
    def it2 = Iterator.range(2, 12)
    def it3 = Iterator.range(3, 13)
    def tlIt = Iterator.range(0, 10).map(i => HList(i, i+1, i+2, i+3))

    equalInferredType(it zipN HList(it1, it2, it3), tlIt)

    (it zipN HList(it1, it2, it3) sameElements tlIt) shouldBe true
  }

  it should "zip one element" in {
    def it = Iterator.range(0, 10)
    def tlIt = Iterator.range(0, 10).map(HList(_))

    equalInferredType(Iterator.zip(HList(it)), tlIt)

    (Iterator.zip(HList(it)) sameElements tlIt) shouldBe true
  }

  it should "zip 3 elements" in {
    def it0 = Iterator.range(0, 10)
    def it1 = Iterator.range(1, 11)
    def it2 = Iterator.range(2, 12)
    def tlIt = Iterator.range(0, 10).map(i => HList(i, i+1, i+2))

    equalInferredType(Iterator.zip(HList(it0, it1, it2)), tlIt)

    (Iterator.zip(HList(it0, it1, it2)) sameElements tlIt) shouldBe true
  }

}

class IteratorSizedExtensionsSpec extends FlatSpec with Matchers {

  "An iterator" should "return HNil when asked its first 0 values" in {
    def it = Iterator.range(0, 5)
    val firstZeroOption = it.takeS[List](0)

    equalInferredTypeAndValue(Option(Sized.empty[List[Int]]), firstZeroOption) shouldBe true
  }

  it should "return its first five values when asked to do so" in {
    def it = Iterator.range(0, 10)
    val firstFiveOption = it.takeS[List](5)

    equalInferredTypeAndValue(Option(Sized[List](0, 1, 2, 3, 4)), firstFiveOption) shouldBe true
  }

  it should "return None when too much elements are taken from it" in {
    def it = Iterator.range(0, 5)
    val firstSixOption = it.takeS[List](6)

    equalInferredTypeAndValue(Option.empty[Sized[List[Int], nat._6]], firstSixOption) shouldBe true
  }


  it should "return HNil when asked a slice from n to n" in {
    def it = Iterator.range(0, 5)
    val sliceOption = it.sliceS[List](2, 2)

    equalInferredTypeAndValue(Option(Sized.empty[List[Int]]), sliceOption) shouldBe true
  }

  it should "not compile when asked a slice from n to n - 1" in {
    def it = Iterator.range(0, 5)
    illTyped("""it.sliceS[List](2, 1)""")
  }

  it should "return the right slice when asked to do so" in {
    def it = Iterator.range(0, 10)
    val sliceOption = it.sliceS[List](2, 6)

    equalInferredTypeAndValue(Option(Sized[List](2, 3, 4, 5)), sliceOption) shouldBe true
  }

  it should "take 3 to the right" in {
    def it = Iterator.range(0, 7)
    val res = it.takeRightS[List](3)

    equalInferredTypeAndValue(res, Option(Sized[List](4, 5, 6))) shouldBe true
  }

  it should "take nothing to the right" in {
    def it = Iterator.range(0, 3)
    val res = it.takeRightS[List](4)

    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._4]]) shouldBe true
  }

  it should "scan 3 from the left" in {
    def it = Iterator.range(0, 7)
    val res = it.scanLeftS[List](0, 3)(_ + _)

    equalInferredTypeAndValue(res, Option(Sized[List](0, 0, 1, 3))) shouldBe true
  }

  it should "scan nothing from the left" in {
    def it = Iterator.range(0, 3)
    val res = it.scanLeftS[List](0, 4)(_ + _)

    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._5]]) shouldBe true
  }

  it should "scan 3 from the right" in {
    def it = Iterator.range(0, 7)
    val res = it.scanRightS[List](0, 3)(_ + _)

    equalInferredTypeAndValue(res, Option(Sized[List](15, 11, 6, 0))) shouldBe true
  }

  it should "scan nothing from the right" in {
    def it = Iterator.range(0, 3)
    val res = it.scanRightS[List](0, 4)(_ + _)

    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._5]]) shouldBe true
  }

  it should "find an empty list of indices" in {
    def it = Iterator.range(0, 7)
    def res = it.indicesWhereS[List](0) { _ > 3 }

    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no indices" in {
    def it = Iterator.range(0, 7)
    def res = it.indicesWhereS[List](2) { _ >= 7 }

    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find three indices" in {
    def it = Iterator.range(0, 7)
    def res = it.indicesWhereS[List](3) { _ % 3 == 0 }

    equalInferredTypeAndValue(res, Option(Sized[List](0, 3, 6))) shouldBe true
  }

  it should "find an empty list of indices of" in {
    def it = Iterator.range(0, 3) ++ Iterator.fill(3)(1)
    def res = it.indicesOfS[List](0, 1)

    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no indices of" in {
    def it = Iterator.range(0, 3) ++ Iterator.fill(3)(1)
    def res = it.indicesOfS[List](2, 3)

    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find three indices of" in {
    def it = Iterator.range(0, 3) ++ Iterator.fill(3)(1)
    def res = it.indicesOfS[List](3, 1)

    equalInferredTypeAndValue(res, Option(Sized[List](1, 3, 4))) shouldBe true
  }

  it should "refuse to be grouped by 0" in {
    def it = Iterator.range(0, 5)

    it.groupedS[List](1)

    illTyped(""" it.groupedS[List](0) """)
  }

  it should "be grouped by 1" in {
    def it = Iterator.range(0, 5)
    def other = Iterator(Sized[List](0), Sized[List](1), Sized[List](2), Sized[List](3), Sized[List](4))

    equalInferredType(it.groupedS[List](1), other)

    (it.groupedS[List](1) sameElements other) shouldBe true
  }

  it should "be grouped by 5" in {
    def it = Iterator.range(0, 10)
    def other = Iterator(Sized[List](0, 1, 2, 3, 4), Sized[List](5, 6, 7, 8, 9))

    equalInferredType(it.groupedS[List](5), other)

    (it.groupedS[List](5) sameElements other) shouldBe true
  }

  it should "be sliding by 2 stepping by 1" in {
    def it = Iterator.range(0, 5)
    def expected = Iterator(Sized[List](0, 1), Sized[List](1, 2), Sized[List](2, 3), Sized[List](3, 4))

    def res = it.slidingS[List](2, 1)

    equalInferredType(res, expected)
    (res sameElements expected) shouldBe true
  }

  it should "be sliding by 2 stepping by 1 by default" in {
    def it = Iterator.range(0, 5)
    def expected = Iterator(Sized[List](0, 1), Sized[List](1, 2), Sized[List](2, 3), Sized[List](3, 4))

    def res = it.slidingS[List](2)

    equalInferredType(res, expected)
    (res sameElements expected) shouldBe true
  }

  it should "be sliding by 3 stepping by 2" in {
    def it = Iterator.range(0, 6)
    def expected = Iterator(Sized[List](0, 1, 2), Sized[List](2, 3, 4))

    def res = it.slidingS[List](3, 2)

    equalInferredType(res, expected)
    (res sameElements expected) shouldBe true
  }

  it should "duplicate 0-times" in {
    def it = Iterator.range(0, 6)
    equalInferredType(it.duplicatesS[List](0), Sized.empty[List[Iterator[Int]]])
  }

  it should "duplicate it-self" in {
    def it = Iterator.range(0, 6)

    equalInferredType(it.duplicatesS[List](1), Sized[List](it))

    (it.duplicatesS[List](1).head sameElements it) shouldBe true
  }

  it should "duplicate it-self 3 times" in {
    def it = Iterator.range(0, 6)

    equalInferredType(it.duplicatesS[List](3), Sized[List](it, it, it))

    val iterators = it.duplicatesS[List](3)
    (iterators(0) sameElements it) shouldBe true
    (iterators(1) sameElements it) shouldBe true
    (iterators(2) sameElements it) shouldBe true
  }

  // This one does not compile, as scalac does not seem too inclined to infer some Nothing type parameters where required    
  //  it should "zip itself" in {
  //    def it = Iterator.range(0, 10)
  //    def sizedIt = Iterator.range(0, 10).map(Sized[List](_))
  //    
  //    equalInferredType(it zipN Sized[List](), sizedIt)
  //
  //    (it zipN Sized[List]() sameElements sizedIt) shouldBe true
  //  }

  it should "zip with another" in {
    def it = Iterator.range(0, 10)
    def it2 = Iterator.range(10, 0, -1)
    def sizedIt = Iterator.range(0, 10).map(i => Sized[List](i, 10 - i))

    equalInferredType(it zipN Sized[List](it2), sizedIt)

    (it zipN Sized[List](it2) sameElements sizedIt) shouldBe true
  }

  it should "zip with 3 other" in {
    def it = Iterator.range(0, 10)
    def it1 = Iterator.range(1, 11)
    def it2 = Iterator.range(2, 12)
    def it3 = Iterator.range(3, 13)
    def tlIt = Iterator.range(0, 10).map(i => Sized[List](i, i+1, i+2, i+3))

    equalInferredType(it zipN Sized[List](it1, it2, it3), tlIt)

    (it zipN Sized[List](it1, it2, it3) sameElements tlIt) shouldBe true
  }

  it should "zip one element" in {
    def it = Iterator.range(0, 10)
    def tlIt = Iterator.range(0, 10).map(Sized[List](_))

    equalInferredType(Iterator.zip(Sized[List](it)), tlIt)

    (Iterator.zip(Sized[List](it)) sameElements tlIt) shouldBe true
  }

  it should "zip 3 elements" in {
    def it0 = Iterator.range(0, 10)
    def it1 = Iterator.range(1, 11)
    def it2 = Iterator.range(2, 12)
    def tlIt = Iterator.range(0, 10).map(i => Sized[List](i, i+1, i+2))

    equalInferredType(Iterator.zip(Sized[List](it0, it1, it2)), tlIt)

    (Iterator.zip(Sized[List](it0, it1, it2)) sameElements tlIt) shouldBe true
  }

}
