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

package shapeless.pimpedstd

import shapeless._, record._
import shapeless.pimpedstd.syntax._
import shapeless.test.illTyped
import org.scalatest._
import shapeless.pimpedstd.test._

class TraversableLikeTupleExtensionsSpec extends FlatSpec with Matchers {

  "A TraversableLike" should "return Unit when asked its first 0 values" in {
    val l = List.range(0, 5)
    val firstZeroOption = l.takeT(0)

    equalInferredTypeAndValue(Option(()), firstZeroOption) shouldBe true
  }

  it should "return its first five values when asked to do so" in {
    val l = List.range(0, 10)
    val firstFiveOption = l.takeT(5)

    equalInferredTypeAndValue(Option((0, 1, 2, 3, 4)), firstFiveOption) shouldBe true
  }

  it should "return None when too much elements are taken from it" in {
    val l = List.range(0, 5)
    val firstSixOption = l.takeT(6)

    val rep6 = util.TupleRepeat(6)
    equalInferredTypeAndValue(Option.empty[rep6.Out[Int]], firstSixOption) shouldBe true
  }

  it should "return Unit when asked a slice from n to n" in {
    val l = List.range(0, 5)
    val sliceOption = l.sliceT(2, 2)

    equalInferredTypeAndValue(Option(()), sliceOption) shouldBe true
  }

  it should "not compile when asked a slice from n to n - 1" in {
    val l = List.range(0, 5)
    illTyped("l.sliceT(2, 1)")
  }

  it should "return the right slice when asked to do so" in {
    val l = List.range(0, 10)
    val sliceOption = l.sliceT(2, 6)

    equalInferredTypeAndValue(Option((2, 3, 4, 5)), sliceOption) shouldBe true
  }

  it should "return None when too much elements are sliced from it" in {
    val l = List.range(0, 5)
    val sliceOption = l.sliceT(4, 6)

    equalInferredTypeAndValue(Option.empty[(Int, Int)], sliceOption) shouldBe true
  }

  it should "split at the beginning" in {
    val l = List.range(0, 5)
    val res = l.splitAtT(0)

    equalInferredTypeAndValue(res, Option(((), l))) shouldBe true
  }

  it should "split at 2" in {
    val l = List.range(0, 5)
    val res = l.splitAtT(2)

    equalInferredTypeAndValue(res, Option(((0, 1), l drop 2))) shouldBe true
  }

  it should "not split" in {
    val l = List.range(0, 5)
    val res = l.splitAtT(6)

    val rep6 = util.TupleRepeat(6)
    equalInferredTypeAndValue(res, Option.empty[(rep6.Out[Int], List[Int])]) shouldBe true
  }

  it should "scan 3 from the left" in {
    val l = List.range(0, 7)
    val res = l.scanLeftT(3, 0)(_ + _)

    equalInferredTypeAndValue(res, Option((0, 0, 1, 3), l drop 3)) shouldBe true
  }

  it should "scan nothing from the left" in {
    val l = List.range(0, 3)
    val res = l.scanLeftT(4, 0)(_ + _)

    val helper = util.TupleRepeat(5)
    equalInferredTypeAndValue(res, Option.empty[(helper.Out[Int], List[Int])]) shouldBe true
  }

  it should "scan 3 from the right" in {
    val l = List.range(0, 7)
    val res = l.scanRightT(3, 0)(_ + _)

    equalInferredTypeAndValue(res, Option(((15, 11, 6, 0), l take 4))) shouldBe true
  }

  it should "scan nothing from the right" in {
    val l = List.range(0, 3)
    val res = l.scanRightT(4, 0)(_ + _)

    val helper = util.TupleRepeat(5)
    equalInferredTypeAndValue(res, Option.empty[(helper.Out[Int], List[Int])]) shouldBe true
  }

}

class TraversableLikeHListExtensionsSpec extends FlatSpec with Matchers {

  "A TraversableLike" should "return HNil when asked its first 0 values" in {
    val l = List.range(0, 5)
    val firstZeroOption = l.takeH(0)

    equalInferredTypeAndValue(Option(HNil: HNil), firstZeroOption) shouldBe true
  }

  it should "return its first five values when asked to do so" in {
    val l = List.range(0, 10)
    val firstFiveOption = l.takeH(5)

    equalInferredTypeAndValue(Option(HList(0, 1, 2, 3, 4)), firstFiveOption) shouldBe true
  }

  it should "return None when too much elements are taken from it" in {
    val l = List.range(0, 5)
    val firstSixOption = l.takeH(6)

    val rep6 = util.HListRepeat(6)
    equalInferredTypeAndValue(Option.empty[rep6.Out[Int]], firstSixOption) shouldBe true
  }

  it should "return HNil when asked a slice from n to n" in {
    val l = List.range(0, 5)
    val sliceOption = l.sliceH(2, 2)

    equalInferredTypeAndValue(Option(HNil: HNil), sliceOption) shouldBe true
  }

  it should "not compile when asked a slice from n to n - 1" in {
    val l = List.range(0, 5)
    illTyped("l.sliceH(2, 1)")
  }

  it should "return the right slice when asked to do so" in {
    val l = List.range(0, 10)
    val sliceOption = l.sliceH(2, 6)

    equalInferredTypeAndValue(Option(HList(2, 3, 4, 5)), sliceOption) shouldBe true
  }

  it should "return None when too much elements are sliced from it" in {
    val l = List.range(0, 5)
    val sliceOption = l.sliceH(4, 6)

    equalInferredTypeAndValue(Option.empty[Int :: Int :: HNil], sliceOption) shouldBe true
  }

  it should "split at the beginning" in {
    val l = List.range(0, 5)
    val res = l.splitAtH(0)

    equalInferredTypeAndValue(res, Option((HNil: HNil, l))) shouldBe true
  }

  it should "split at 2" in {
    val l = List.range(0, 5)
    val res = l.splitAtH(2)

    equalInferredTypeAndValue(res, Option((HList(0, 1), l drop 2))) shouldBe true
  }

  it should "not split" in {
    val l = List.range(0, 5)
    val res = l.splitAtH(6)

    val rep6 = util.HListRepeat(6)
    equalInferredTypeAndValue(res, Option.empty[(rep6.Out[Int], List[Int])]) shouldBe true
  }

  it should "scan 3 from the left" in {
    val l = List.range(0, 7)
    val res = l.scanLeftH(3, 0)(_ + _)

    equalInferredTypeAndValue(res, Option(HList(0, 0, 1, 3), l drop 3)) shouldBe true
  }

  it should "scan nothing from the left" in {
    val l = List.range(0, 3)
    val res = l.scanLeftH(4, 0)(_ + _)

    val helper = util.HListRepeat(5)
    equalInferredTypeAndValue(res, Option.empty[(helper.Out[Int], List[Int])]) shouldBe true
  }

  it should "scan 3 from the right" in {
    val l = List.range(0, 7)
    val res = l.scanRightH(3, 0)(_ + _)

    equalInferredTypeAndValue(res, Option((HList(15, 11, 6, 0), l take 4))) shouldBe true
  }

  it should "scan nothing from the right" in {
    val l = List.range(0, 3)
    val res = l.scanRightH(4, 0)(_ + _)

    val helper = util.HListRepeat(5)
    equalInferredTypeAndValue(res, Option.empty[(helper.Out[Int], List[Int])]) shouldBe true
  }

}

class TraversableLikeSizedExtensionsSpec extends FlatSpec with Matchers {

  "A TraversableLike" should "return an empty Sized when asked its first 0 values" in {
    val l = List.range(0, 5)
    val firstZeroOption = l.takeS(0)

    equalInferredTypeAndValue(Option(Sized.empty[List[Int]]), firstZeroOption) shouldBe true
  }

  it should "return its first five values when asked to do so" in {
    val l = List.range(0, 10)
    val firstFiveOption = l.takeS(5)

    equalInferredTypeAndValue(Option(Sized[List](0, 1, 2, 3, 4)), firstFiveOption) shouldBe true
  }

  it should "return None when too much elements are taken from it" in {
    val l = List.range(0, 5)
    val firstSixOption = l.takeS(6)

    equalInferredTypeAndValue(Option.empty[Sized[List[Int], nat._6]], firstSixOption) shouldBe true
  }

  it should "return an empty Sized when asked a slice from n to n" in {
    val l = List.range(0, 5)
    val sliceOption = l.sliceS(2, 2)

    equalInferredTypeAndValue(Option(Sized.empty[List[Int]]), sliceOption) shouldBe true
  }

  it should "not compile when asked a slice from n to n - 1" in {
    val l = List.range(0, 5)
    illTyped("l.sliceS(2, 1)")
  }

  it should "return the right slice when asked to do so" in {
    val l = List.range(0, 10)
    val sliceOption = l.sliceS(2, 6)

    equalInferredTypeAndValue(Option(Sized[List](2, 3, 4, 5)), sliceOption) shouldBe true
  }

  it should "return None when too much elements are sliced from it" in {
    val l = List.range(0, 5)
    val sliceOption = l.sliceS(4, 6)

    equalInferredTypeAndValue(Option.empty[Sized[List[Int], nat._2]], sliceOption) shouldBe true
  }

  it should "split at the beginning" in {
    val l = List.range(0, 5)
    val res = l.splitAtS(0)

    equalInferredTypeAndValue(res, Option((Sized.empty[List[Int]], l))) shouldBe true
  }

  it should "split at 2" in {
    val l = List.range(0, 5)
    val res = l.splitAtS(2)

    equalInferredTypeAndValue(res, Option((Sized[List](0, 1), l drop 2))) shouldBe true
  }

  it should "not split" in {
    val l = List.range(0, 5)
    val res = l.splitAtS(6)

    equalInferredTypeAndValue(res, Option.empty[(Sized[List[Int], nat._6], List[Int])]) shouldBe true
  }

  it should "scan 3 from the left" in {
    val l = List.range(0, 7)
    val res = l.scanLeftS(3, 0)(_ + _)

    equalInferredTypeAndValue(res, Option(Sized[List](0, 0, 1, 3), l drop 3)) shouldBe true
  }

  it should "scan nothing from the left" in {
    val l = List.range(0, 3)
    val res = l.scanLeftS(4, 0)(_ + _)

    equalInferredTypeAndValue(res, Option.empty[(Sized[List[Int], nat._5], List[Int])]) shouldBe true
  }

  it should "scan 3 from the right" in {
    val l = List.range(0, 7)
    val res = l.scanRightS(3, 0)(_ + _)

    equalInferredTypeAndValue(res, Option((Sized[List](15, 11, 6, 0), l take 4))) shouldBe true
  }

  it should "scan nothing from the right" in {
    val l = List.range(0, 3)
    val res = l.scanRightS(4, 0)(_ + _)

    equalInferredTypeAndValue(res, Option.empty[(Sized[List[Int], nat._5], List[Int])]) shouldBe true
  }

}

class TraversableLikeExtraExtensionsSpec extends FlatSpec with Matchers {

  sealed trait Base
  case object Single extends Base
  final case class Other(dummy: Int) extends Base
  
  
  "A TraversableLike" should "partition as ADT" in {
    val l = List[Base](Other(2), Other(4), Single, Other(2), Single)
    val res = l.typePartition
    
    equalInferredType(res('Single), List[Single.type](Single, Single))
    equalInferredType(res('Other), List[Other](Other(2), Other(4), Other(2)))
  }

  it should "partition with ADT" in {
    val l = List.range(0, 4)
    val res = l.typePartitionAs {
      case 0 => Single: Base
      case n => Other(n): Base
    }

    equalInferredType(res('Single), List[Single.type](Single, Single))
    equalInferredType(res('Other), List[Other](Other(2), Other(4), Other(2)))
  }

  it should "partition along with ADT" in {
    val l = List.range(0, 4)
    val res = l.typePartitionWith {
      case 0 => Single: Base
      case n => Other(n): Base
    }

    equalInferredType(res('Single), List[(Single.type, Int)](Single -> 0, Single -> 0))
    equalInferredType(res('Other), List[(Other, Int)](Other(2) -> 2, Other(4) -> 4, Other(2) -> 2))
  }

}
