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
import shapeless.refinedstd.util.{ HListRepeat, TupleRepeat }
import org.scalatest._
import shapeless.refinedstd.test._

class TraversableOnceTupleExtensionsSpec extends FlatSpec with Matchers {

  "A TraversableOnce" should "return Unit when asked to collect the first 0 values" in {
    val l = List(0, false, "1", 0.0, "b", "last")
    val firstZeroOption = l.collectFirstT(0) { case s: String => s }

    equalInferredTypeAndValue(Option(()), firstZeroOption) shouldBe true
  }

  it should "collect the first 2 values when asked to do so" in {
    val l = List(0, false, "1", 0.0, "b", "last")
    val firstTwoOption = l.collectFirstT(2) { case s: String => s }

    equalInferredTypeAndValue(Option(("1", "b")), firstTwoOption) shouldBe true
  }

  it should "collect nothing if asked too many values" in {
    val l = List(0, false, "1", 0.0, "b", "last")
    val firstFourOption = l.collectFirstT(4) { case d: Double => d }

    val helper = TupleRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[Double]], firstFourOption) shouldBe true
  }

  it should "return Unit when asked to find 0 values" in {
    val l = List.range(0, 5)
    val res = l.findT(0) { _ < 3 }

    equalInferredTypeAndValue(Option(()), res) shouldBe true
  }

  it should "find the first 2 values when asked to do so" in {
    val l = List.range(0, 5)
    val res = l.findT(2) { _ < 3 }

    equalInferredTypeAndValue(Option((0, 1)), res) shouldBe true
  }

  it should "find nothing if asked too many values" in {
    val l = List.range(0, 5)
    val res = l.findT(4) { _ < 3 }

    val helper = TupleRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[Int]], res) shouldBe true
  }

  it should "return Unit when asked 0 minimum values" in {
    val l = List(1, 3, 4, 1, 5)
    val res = l.minT(0)

    equalInferredTypeAndValue(Option(()), res) shouldBe true
  }

  it should "find 2 minimum values when asked to do so" in {
    val l = List(1, 3, 4, 0, 5)
    val res = l.minT(2)

    equalInferredTypeAndValue(Option((0, 1)), res) shouldBe true
  }

  it should "find nothing if asked too many minima" in {
    val l = List(1, 3, 4)
    val res = l.minT(4)

    val helper = TupleRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[Int]], res) shouldBe true
  }

  it should "return Unit when asked 0 minimum-by values" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 1, "z" -> 5)
    val res = l.minTBy(0) { case (_, v) => v }

    equalInferredTypeAndValue(Option(()), res) shouldBe true
  }

  it should "find 2 minimum-by values when asked to do so" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 0, "z" -> 5)
    val res = l.minTBy(2) { case (_, v) => v }

    equalInferredTypeAndValue(Option(("f" -> 0, "a" -> 1)), res) shouldBe true
  }

  it should "find nothing if asked too many minima-by" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4)
    val res = l.minTBy(4) { case (_, v) => v }

    val helper = TupleRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[(String, Int)]], res) shouldBe true
  }

  it should "return Unit when asked 0 maximum values" in {
    val l = List(1, 3, 4, 1, 5)
    val res = l.maxT(0)

    equalInferredTypeAndValue(Option(()), res) shouldBe true
  }

  it should "find 2 maximum values when asked to do so" in {
    val l = List(1, 3, 4, 0, 5)
    val res = l.maxT(2)

    equalInferredTypeAndValue(Option((5, 4)), res) shouldBe true
  }

  it should "find nothing if asked too many maxima" in {
    val l = List(1, 3, 4)
    val res = l.maxT(4)

    val helper = TupleRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[Int]], res) shouldBe true
  }

  it should "return Unit when asked 0 maximum-by values" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 1, "z" -> 5)
    val res = l.maxTBy(0) { case (_, v) => v }

    equalInferredTypeAndValue(Option(()), res) shouldBe true
  }

  it should "find 2 maximum-by values when asked to do so" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 0, "z" -> 5)
    val res = l.maxTBy(2) { case (_, v) => v }

    equalInferredTypeAndValue(Option(("z" -> 5, "g" -> 4)), res) shouldBe true
  }

  it should "find nothing if asked too many maxima-by" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4)
    val res = l.maxTBy(4) { case (_, v) => v }

    val helper = TupleRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[(String, Int)]], res) shouldBe true
  }

}

class TraversableOnceHListExtensionsSpec extends FlatSpec with Matchers {

  "A TraversableOnce" should "return HNil when asked to collect the first 0 values" in {
    val l = List(0, false, "1", 0.0, "b", "last")
    val firstZeroOption = l.collectFirstH(0) { case s: String => s }

    equalInferredTypeAndValue(Option(HNil: HNil), firstZeroOption) shouldBe true
  }

  it should "collect the first 2 values when asked to do so" in {
    val l = List(0, false, "1", 0.0, "b", "last")
    val firstTwoOption = l.collectFirstH(2) { case s: String => s }

    equalInferredTypeAndValue(Option(HList("1", "b")), firstTwoOption) shouldBe true
  }

  it should "collect nothing if asked too many values" in {
    val l = List(0, false, "1", 0.0, "b", "last")
    val firstFourOption = l.collectFirstH(4) { case d: Double => d }

    val helper = HListRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[Double]], firstFourOption) shouldBe true
  }

  it should "return HNil when asked to find 0 values" in {
    val l = List.range(0, 5)
    val res = l.findH(0) { _ < 3 }

    equalInferredTypeAndValue(Option(HNil: HNil), res) shouldBe true
  }

  it should "find the first 2 values when asked to do so" in {
    val l = List.range(0, 5)
    val res = l.findH(2) { _ < 3 }

    equalInferredTypeAndValue(Option(HList(0, 1)), res) shouldBe true
  }

  it should "find nothing if asked too many values" in {
    val l = List.range(0, 5)
    val res= l.findH(4) { _ < 3 }

    val helper = HListRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[Int]], res) shouldBe true
  }

  it should "return HNil when asked 0 minimum values" in {
    val l = List(1, 3, 4, 1, 5)
    val res = l.minH(0)

    equalInferredTypeAndValue(Option(HNil: HNil), res) shouldBe true
  }

  it should "find 2 minimum values when asked to do so" in {
    val l = List(1, 3, 4, 0, 5)
    val res = l.minH(2)

    equalInferredTypeAndValue(Option(HList(0, 1)), res) shouldBe true
  }

  it should "find nothing if asked too many minima" in {
    val l = List(1, 3, 4)
    val res = l.minH(4)

    val helper = HListRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[Int]], res) shouldBe true
  }

  it should "return HNil when asked 0 minimum-by values" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 1, "z" -> 5)
    val res = l.minHBy(0) { case (_, v) => v }

    equalInferredTypeAndValue(Option(HNil: HNil), res) shouldBe true
  }

  it should "find 2 minimum-by values when asked to do so" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 0, "z" -> 5)
    val res = l.minHBy(2) { case (_, v) => v }

    equalInferredTypeAndValue(Option(HList("f" -> 0, "a" -> 1)), res) shouldBe true
  }

  it should "find nothing if asked too many minima-by" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4)
    val res = l.minHBy(4) { case (_, v) => v }

    val helper = HListRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[(String, Int)]], res) shouldBe true
  }

  it should "return HNil when asked 0 maximum values" in {
    val l = List(1, 3, 4, 1, 5)
    val res = l.maxH(0)

    equalInferredTypeAndValue(Option(HNil: HNil), res) shouldBe true
  }

  it should "find 2 maximum values when asked to do so" in {
    val l = List(1, 3, 4, 0, 5)
    val res = l.maxH(2)

    equalInferredTypeAndValue(Option(HList(5, 4)), res) shouldBe true
  }

  it should "find nothing if asked too many maxima" in {
    val l = List(1, 3, 4)
    val res = l.maxH(4)

    val helper = HListRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[Int]], res) shouldBe true
  }

  it should "return HNil when asked 0 maximum-by values" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 1, "z" -> 5)
    val res = l.maxHBy(0) { case (_, v) => v }

    equalInferredTypeAndValue(Option(HNil: HNil), res) shouldBe true
  }

  it should "find 2 maximum-by values when asked to do so" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 0, "z" -> 5)
    val res = l.maxHBy(2) { case (_, v) => v }

    equalInferredTypeAndValue(Option(HList("z" -> 5, "g" -> 4)), res) shouldBe true
  }

  it should "find nothing if asked too many maxima-by" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4)
    val res = l.maxHBy(4) { case (_, v) => v }

    val helper = HListRepeat(4)
    equalInferredTypeAndValue(Option.empty[helper.Out[(String, Int)]], res) shouldBe true
  }

}

class TraversableOnceSizedExtensionsSpec extends FlatSpec with Matchers {

  "A TraversableOnce" should "return an empty Sized when asked to collect the first 0 values" in {
    val l = List(0, false, "1", 0.0, "b", "last")
    val firstZeroOption = l.collectFirstS(0) { case s: String => s }

    equalInferredTypeAndValue(Option(Sized.empty[List[String]]), firstZeroOption) shouldBe true
  }

  it should "collect the first 2 values when asked to do so" in {
    val l = List(0, false, "1", 0.0, "b", "last")
    val firstTwoOption = l.collectFirstS(2) { case s: String => s }

    equalInferredTypeAndValue(Option(Sized[List]("1", "b")), firstTwoOption) shouldBe true
  }

  it should "collect nothing if asked too many values" in {
    val l = List(0, false, "1", 0.0, "b", "last")
    val firstFourOption = l.collectFirstS(4) { case d: Double => d }

    equalInferredTypeAndValue(Option.empty[Sized[List[Double], nat._4]], firstFourOption) shouldBe true
  }

  it should "return an empty Sized when asked to find 0 values" in {
    val l = List.range(0, 5)
    val res = l.findS(0) { _ < 3 }

    equalInferredTypeAndValue(Option(Sized.empty[List[Int]]), res) shouldBe true
  }

  it should "find the first 2 values when asked to do so" in {
    val l = List.range(0, 5)
    val res = l.findS(2) { _ < 3 }

    equalInferredTypeAndValue(Option(Sized[List](0, 1)), res) shouldBe true
  }

  it should "find nothing if asked too many values" in {
    val l = List.range(0, 5)
    val res = l.findS(4) { _ < 3 }

    equalInferredTypeAndValue(Option.empty[Sized[List[Int], nat._4]], res) shouldBe true
  }

  it should "return an empty Sized when asked 0 minimum values" in {
    val l = List(1, 3, 4, 1, 5)
    val res = l.minS(0)

    equalInferredTypeAndValue(Option(Sized.empty[List[Int]]), res) shouldBe true
  }

  it should "find 2 minimum values when asked to do so" in {
    val l = List(1, 3, 4, 0, 5)
    val res = l.minS(2)

    equalInferredTypeAndValue(Option(Sized[List](0, 1)), res) shouldBe true
  }

  it should "find nothing if asked too many minima" in {
    val l = List(1, 3, 4)
    val res = l.minS(4)

    equalInferredTypeAndValue(Option.empty[Sized[List[Int], nat._4]], res) shouldBe true
  }

  it should "return an empty Sized when asked 0 minimum-by values" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 1, "z" -> 5)
    val res = l.minSBy(0) { case (_, v) => v }

    equalInferredTypeAndValue(Option(Sized.empty[List[(String, Int)]]), res) shouldBe true
  }

  it should "find 2 minimum-by values when asked to do so" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 0, "z" -> 5)
    val res = l.minSBy(2) { case (_, v) => v }

    equalInferredTypeAndValue(Option(Sized[List]("f" -> 0, "a" -> 1)), res) shouldBe true
  }

  it should "find nothing if asked too many minima-by" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4)
    val res = l.minSBy(4) { case (_, v) => v }

    equalInferredTypeAndValue(Option.empty[Sized[List[(String, Int)], nat._4]], res) shouldBe true
  }

  it should "return an empty Sized when asked 0 maximum values" in {
    val l = List(1, 3, 4, 1, 5)
    val res = l.maxS(0)

    equalInferredTypeAndValue(Option(Sized.empty[List[Int]]), res) shouldBe true
  }

  it should "find 2 maximum values when asked to do so" in {
    val l = List(1, 3, 4, 0, 5)
    val res = l.maxS(2)

    equalInferredTypeAndValue(Option(Sized[List](5, 4)), res) shouldBe true
  }

  it should "find nothing if asked too many maxima" in {
    val l = List(1, 3, 4)
    val res = l.maxS(4)

    equalInferredTypeAndValue(Option.empty[Sized[List[Int], nat._4]], res) shouldBe true
  }

  it should "return an empty Sized when asked 0 maximum-by values" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 1, "z" -> 5)
    val res = l.maxSBy(0) { case (_, v) => v }

    equalInferredTypeAndValue(Option(Sized.empty[List[(String, Int)]]), res) shouldBe true
  }

  it should "find 2 maximum-by values when asked to do so" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 0, "z" -> 5)
    val res = l.maxSBy(2) { case (_, v) => v }

    equalInferredTypeAndValue(Option(Sized[List]("z" -> 5, "g" -> 4)), res) shouldBe true
  }

  it should "find nothing if asked too many maxima-by" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4)
    val res = l.maxSBy(4) { case (_, v) => v }

    val helper = HListRepeat(4)
    equalInferredTypeAndValue(Option.empty[Sized[List[(String, Int)], nat._4]], res) shouldBe true
  }

}

class TraversableOnceExtraExtensionsSpec extends FlatSpec with Matchers {

  "A TraversableOnce" should "return true when asked if 0 values exist" in {
    val l = List.range(0, 5)
    l.exist(0)(_ > 5) shouldBe true
  }

  it should "find enough existing values" in {
    val l = List.range(0, 5)
    l.exist(3)(_ > 1) shouldBe true
  }

  it should "not find enough existing values" in {
    val l = List.range(0, 5)
    l.exist(3)(_ > 2) shouldBe false
  }
  
  it should "find a minimum" in {
    val l = List.range(0, 5)
    l.minOption shouldBe Some(0)
  }

  it should "not find a minimum" in {
    val l = List.empty[String]
    l.minOption shouldBe None
  }

  it should "find a minimum-by" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 0, "z" -> 5)
    l.minByOption { case (_, v) => v } shouldBe Some("f" -> 0)
  }

  it should "not find a minimum-by" in {
    val l = List.empty[(String, Int)]
    l.minByOption { case (_, v) => v } shouldBe None
  }

  it should "find a maximum" in {
    val l = List.range(0, 5)
    l.maxOption shouldBe Some(4)
  }

  it should "not find a maximum" in {
    val l = List.empty[String]
    l.maxOption shouldBe None
  }

  it should "find a maximum-by" in {
    val l = List("a" -> 1, "b" -> 3, "g" -> 4, "f" -> 0, "z" -> 5)
    l.maxByOption { case (_, v) => v } shouldBe Some("z" -> 5)
  }

  it should "not find a maximum-by" in {
    val l = List.empty[(String, Int)]
    l.maxByOption { case (_, v) => v } shouldBe None
  }

}
