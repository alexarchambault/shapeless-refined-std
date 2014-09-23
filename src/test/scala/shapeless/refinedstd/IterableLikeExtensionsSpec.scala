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

import shapeless.{ HList, HNil, Sized }
import shapeless.refinedstd.syntax._
import shapeless.test.illTyped
import org.scalatest._
import shapeless.refinedstd.test._

class IterableLikeTupleExtensionsSpec extends FlatSpec with Matchers {

  "An IterableLike" should "refuse to be grouped by 0" in {
    val l = List.range(0, 5)
    illTyped(""" l.groupedT(0) """)
  }

  it should "be grouped by 2" in {
    val l = List.range(0, 5)

    val res = l.groupedT(2)

    equalInferredTypeAndValue(res, List((0, 1), (2, 3))) shouldBe true
  }

  it should "be sliding by 2 stepping by 1" in {
    val l = List.range(0, 5)

    val res = l.slidingT(2, 1)

    equalInferredTypeAndValue(res, List((0, 1), (1, 2), (2, 3), (3, 4))) shouldBe true
  }

  it should "be sliding by 2 stepping by 1 by default" in {
    val l = List.range(0, 5)

    val res = l.slidingT(2)

    equalInferredTypeAndValue(res, List((0, 1), (1, 2), (2, 3), (3, 4))) shouldBe true
  }

  it should "be sliding by 3 stepping by 2" in {
    val l = List.range(0, 6)

    val res = l.slidingT(3, 2)

    equalInferredTypeAndValue(res, List((0, 1, 2), (2, 3, 4))) shouldBe true
  }

  it should "take 3 to the right" in {
    val l = List.range(0, 7)

    val res = l.takeRightT(3)

    equalInferredTypeAndValue(res, Option((4, 5, 6))) shouldBe true
  }

  it should "zip itself" in {
    val it = Iterable.range(0, 10)
    val pIt = Iterable.range(0, 10).map(Tuple1(_))

    equalInferredTypeAndValue(it.zipN(()), pIt) shouldBe true
  }

  it should "zip with another" in {
    val it = Iterable.range(0, 10)
    val it2 = Iterable.range(10, 0, -1)
    val tlIt = Iterable.range(0, 10).map(i => (i, 10 - i))

    equalInferredTypeAndValue(it zipN Tuple1(it2), tlIt) shouldBe true
  }

  it should "zip with 3 other" in {
    val it = Iterable.range(0, 10)
    val it1 = Iterable.range(1, 11)
    val it2 = Iterable.range(2, 12)
    val it3 = Iterable.range(3, 13)
    val tlIt = Iterable.range(0, 10).map(i => (i, i+1, i+2, i+3))

    equalInferredTypeAndValue(it zipN (it1, it2, it3), tlIt) shouldBe true
  }

  it should "zip one element" in {
    val it = Iterable.range(0, 10)
    val tlIt = Iterable.range(0, 10).map(Tuple1(_))

    equalInferredTypeAndValue(Iterable zip Tuple1(it), tlIt) shouldBe true
  }

  it should "zip 3 elements" in {
    val it0 = Iterable.range(0, 10)
    val it1 = Iterable.range(1, 11)
    val it2 = Iterable.range(2, 12)
    val tlIt = Iterable.range(0, 10).map(i => (i, i+1, i+2))

    equalInferredTypeAndValue(Iterable zip (it0, it1, it2), tlIt) shouldBe true
  }

}

class IterableLikeHListExtensionsSpec extends FlatSpec with Matchers {

  "An IterableLike" should "refuse to be grouped by 0" in {
    val l = List.range(0, 5)
    illTyped(""" l.groupedH(0) """)
  }

  it should "be grouped by 2" in {
    val l = List.range(0, 5)

    val res = l.groupedH(2)

    equalInferredTypeAndValue(res, List(0 :: 1 :: HNil, 2 :: 3 :: HNil)) shouldBe true
  }

  it should "be sliding by 2 stepping by 1" in {
    val l = List.range(0, 5)

    val res = l.slidingH(2, 1)

    equalInferredTypeAndValue(res, List(HList(0, 1), HList(1, 2), HList(2, 3), HList(3, 4))) shouldBe true
  }

  it should "be sliding by 2 stepping by 1 by default" in {
    val l = List.range(0, 5)

    val res = l.slidingH(2)

    equalInferredTypeAndValue(res, List(HList(0, 1), HList(1, 2), HList(2, 3), HList(3, 4))) shouldBe true
  }

  it should "be sliding by 3 stepping by 2" in {
    val l = List.range(0, 6)

    val res = l.slidingH(3, 2)

    equalInferredTypeAndValue(res, List(HList(0, 1, 2), HList(2, 3, 4))) shouldBe true
  }

  it should "take 3 to the right" in {
    val l = List.range(0, 7)

    val res = l.takeRightH(3)

    equalInferredTypeAndValue(res, Option(HList(4, 5, 6))) shouldBe true
  }

  it should "zip itself" in {
    val it = Iterable.range(0, 10)
    val hlIt = Iterable.range(0, 10).map(_ :: HNil)

    equalInferredTypeAndValue(it.zipN(HNil), hlIt) shouldBe true
  }

  it should "zip with another" in {
    val it = Iterable.range(0, 10)
    val it2 = Iterable.range(10, 0, -1)
    val tlIt = Iterable.range(0, 10).map(i => i :: (10 - i) :: HNil)

    equalInferredTypeAndValue(it zipN (it2 :: HNil), tlIt) shouldBe true
  }

  it should "zip with 3 other" in {
    val it = Iterable.range(0, 10)
    val it1 = Iterable.range(1, 11)
    val it2 = Iterable.range(2, 12)
    val it3 = Iterable.range(3, 13)
    val tlIt = Iterable.range(0, 10).map(i => HList(i, i+1, i+2, i+3))

    equalInferredTypeAndValue(it zipN HList(it1, it2, it3), tlIt) shouldBe true
  }

  it should "zip one element" in {
    val it = Iterable.range(0, 10)
    val tlIt = Iterable.range(0, 10).map(HList(_))

    equalInferredTypeAndValue(Iterable zip HList(it), tlIt) shouldBe true
  }

  it should "zip 3 elements" in {
    val it0 = Iterable.range(0, 10)
    val it1 = Iterable.range(1, 11)
    val it2 = Iterable.range(2, 12)
    val tlIt = Iterable.range(0, 10).map(i => HList(i, i+1, i+2))

    equalInferredTypeAndValue(Iterable zip HList(it0, it1, it2), tlIt) shouldBe true
  }

}

class IterableLikeSizedExtensionsSpec extends FlatSpec with Matchers {

  "An IterableLike" should "refuse to be grouped by 0" in {
    val l = List.range(0, 5)
    illTyped(""" l.groupedS(0) """)
  }

  it should "be grouped by 2" in {
    val l = List.range(0, 5)

    val res = l.groupedS(2)

    equalInferredTypeAndValue(res, List(Sized[List](0, 1), Sized[List](2, 3))) shouldBe true
  }

  it should "be sliding by 2 stepping by 1" in {
    val l = List.range(0, 5)

    val res = l.slidingS(2, 1)

    equalInferredTypeAndValue(res, List(Sized[List](0, 1), Sized[List](1, 2), Sized[List](2, 3), Sized[List](3, 4))) shouldBe true
  }

  it should "be sliding by 2 stepping by 1 by default" in {
    val l = List.range(0, 5)

    val res = l.slidingS(2)

    equalInferredTypeAndValue(res, List(Sized[List](0, 1), Sized[List](1, 2), Sized[List](2, 3), Sized[List](3, 4))) shouldBe true
  }

  it should "be sliding by 3 stepping by 2" in {
    val l = List.range(0, 6)

    val res = l.slidingS(3, 2)

    equalInferredTypeAndValue(res, List(Sized[List](0, 1, 2), Sized[List](2, 3, 4))) shouldBe true
  }

  it should "take 3 to the right" in {
    val l = List.range(0, 7)

    val res = l.takeRightS(3)

    equalInferredTypeAndValue(res, Option(Sized[List](4, 5, 6))) shouldBe true
  }

  // This one does not compile, as scalac does not seem too inclined to infer some Nothing type parameters where required
  // See also IteratorSizingOpsSpec
  //  it should "zip itself" in {
  //    val it = Iterable.range(0, 10)
  //    val pIt = Iterable.range(0, 10).map(Sized[List](_))
  //
  //    equalInferredTypeAndValue(it.zipN(Sized[List]()), pIt) shouldBe true
  //  }

  it should "zip with another" in {
    val it = Iterable.range(0, 10)
    val it2 = Iterable.range(10, 0, -1)
    val tlIt = Iterable.range(0, 10).map(i => Sized[List](i, 10 - i))

    equalInferredTypeAndValue(it zipN Sized[List](it2), tlIt) shouldBe true
  }

  it should "zip with 3 other" in {
    val it = Iterable.range(0, 10)
    val it1 = Iterable.range(1, 11)
    val it2 = Iterable.range(2, 12)
    val it3 = Iterable.range(3, 13)
    val tlIt = Iterable.range(0, 10).map(i => Sized[List](i, i+1, i+2, i+3))

    equalInferredTypeAndValue(it zipN Sized[List](it1, it2, it3), tlIt) shouldBe true
  }

  it should "zip one element" in {
    val it = Iterable.range(0, 10)
    val tlIt = Iterable.range(0, 10).map(Sized[List](_))

    equalInferredTypeAndValue(Iterable zip Sized[List](it), tlIt) shouldBe true
  }

  it should "zip 3 elements" in {
    val it0 = Iterable.range(0, 10)
    val it1 = Iterable.range(1, 11)
    val it2 = Iterable.range(2, 12)
    val tlIt = Iterable.range(0, 10).map(i => Sized[List](i, i+1, i+2))

    equalInferredTypeAndValue(Iterable zip Sized[List](it0, it1, it2), tlIt) shouldBe true
  }

}
