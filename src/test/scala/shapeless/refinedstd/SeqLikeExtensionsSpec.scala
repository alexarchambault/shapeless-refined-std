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
import shapeless.test.illTyped
import shapeless.syntax.std.tuple._
import shapeless.refinedstd.syntax._
import org.scalatest._
import shapeless.refinedstd.test._

class SeqLikeTupleExtensionsSpec extends FlatSpec with Matchers {

  "A SeqLike" should "not combine by 0" in {
    val l = List.range(0, 5)
    illTyped(""" l.combinationsT(0) """)
  }
  
  it should "combine by 1" in {
    val l = List.range(0, 5)
    val c = l.combinationsT(1)
    
    equalInferredType(c, List.empty[Tuple1[Int]])
    
    c.lengthCompare(l.length) shouldBe 0
    c.toSet shouldBe l.map(Tuple1(_)).toSet
  }

  it should "combine by 2" in {
    val l = List.range(0, 4)
    val c = l.combinationsT(2)

    equalInferredType(c, List.empty[(Int, Int)])

    c.lengthCompare(l.length * (l.length - 1) / 2) shouldBe 0
    c.toSet shouldBe Set((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3))
  }

  it should "combine by 2 iterating" in {
    val l = List.range(0, 4)
    val c = l.combinationsIteratorT(2).toList

    equalInferredType(c, List.empty[(Int, Int)])

    c.lengthCompare(l.length * (l.length - 1) / 2) shouldBe 0
    c.toSet shouldBe Set((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3))
  }
  
  it should "find 0 indices" in {
    val l = List.range(0, 4)
    val res = l.indicesOfT(0, 1)
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }

  it should "find no indices" in {
    val l = List.range(0, 4)
    val res = l.indicesOfT(2, 5)
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find not enough indices" in {
    val l = List.range(0, 4)
    val res = l.indicesOfT(3, 1)
    equalInferredTypeAndValue(res, Option.empty[(Int, Int, Int)]) shouldBe true
  }

  it should "find 2 indices" in {
    val l = 2 :: List.range(0, 4)
    val res = l.indicesOfT(2, 2)
    equalInferredTypeAndValue(res, Option((0, 3))) shouldBe true
  }

  it should "find 1 index" in {
    val l = 2 :: List.range(0, 4)
    val res = l.indicesOfT(1, 2, 1)
    equalInferredTypeAndValue(res, Option(Tuple1(3))) shouldBe true
  }

  it should "find 0 last indices" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfT(0, 1)
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }

  it should "find no last indices" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfT(2, 5)
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find not enough last indices" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfT(3, 1)
    equalInferredTypeAndValue(res, Option.empty[(Int, Int, Int)]) shouldBe true
  }

  it should "find 2 last indices" in {
    val l = 2 :: List.range(0, 4)
    val res = l.lastIndicesOfT(2, 2)
    equalInferredTypeAndValue(res, Option((3, 0))) shouldBe true
  }

  it should "find 1 last index" in {
    val l = 3 :: List.range(0, 4)
    val res = l.lastIndicesOfT(1, 3, 3)
    equalInferredTypeAndValue(res, Option(Tuple1(0))) shouldBe true
  }

  it should "find 0 indices of slice" in {
    val l = List.range(0, 4)
    val res = l.indicesOfSliceT(0, List(1, 2))
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }

  it should "find no indices of slice" in {
    val l = List.range(0, 4)
    val res = l.indicesOfSliceT(2, List(1, 0))
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find not enough indices of slice" in {
    val l = List.range(0, 4)
    val res = l.indicesOfSliceT(2, List(1, 2))
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find 2 indices of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.indicesOfSliceT(2, List(1, 2))
    equalInferredTypeAndValue(res, Option((1, 4))) shouldBe true
  }

  it should "find 1 index of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.indicesOfSliceT(1, List(1, 2), 2)
    equalInferredTypeAndValue(res, Option(Tuple1(4))) shouldBe true
  }

  it should "find 0 last indices of slice" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfSliceT(0, List(0, 1))
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }

  it should "find no last indices of slice" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfSliceT(2, List(1, 0))
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find not enough last indices of slice" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfSliceT(2, List(1, 2))
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find 2 last indices of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.lastIndicesOfSliceT(2, List(1, 2))
    equalInferredTypeAndValue(res, Option((4, 1))) shouldBe true
  }

  it should "find 1 last index of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.lastIndicesOfSliceT(1, List(1, 2), 3)
    equalInferredTypeAndValue(res, Option(Tuple1(1))) shouldBe true
  }

}

class SeqLikeHListExtensionsSpec extends FlatSpec with Matchers {

  "A SeqLike" should "not combine by 0" in {
    val l = List.range(0, 5)
    illTyped(""" l.combinationsH(0) """)
  }

  it should "combine by 1" in {
    val l = List.range(0, 5)
    val c = l.combinationsH(1)

    equalInferredType(c, List.empty[Int :: HNil])

    c.lengthCompare(l.length) shouldBe 0
    c.toSet shouldBe l.map(HList(_)).toSet
  }

  it should "combine by 2" in {
    val l = List.range(0, 4)
    val c = l.combinationsH(2)

    equalInferredType(c, List.empty[Int :: Int :: HNil])

    c.lengthCompare(l.length * (l.length - 1) / 2) shouldBe 0
    c.toSet shouldBe Set(HList(0, 1), HList(0, 2), HList(0, 3), HList(1, 2), HList(1, 3), HList(2, 3))
  }

  it should "combine by 2 iterating" in {
    val l = List.range(0, 4)
    val c = l.combinationsIteratorH(2).toList

    equalInferredType(c, List.empty[Int :: Int :: HNil])

    c.lengthCompare(l.length * (l.length - 1) / 2) shouldBe 0
    c.toSet shouldBe Set(HList(0, 1), HList(0, 2), HList(0, 3), HList(1, 2), HList(1, 3), HList(2, 3))
  }

  it should "find 0 indices" in {
    val l = List.range(0, 4)
    val res = l.indicesOfH(0, 1)
    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no indices" in {
    val l = List.range(0, 4)
    val res = l.indicesOfH(2, 5)
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find not enough indices" in {
    val l = List.range(0, 4)
    val res = l.indicesOfH(3, 1)
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: Int :: HNil]) shouldBe true
  }

  it should "find 2 indices" in {
    val l = 2 :: List.range(0, 4)
    val res = l.indicesOfH(2, 2)
    equalInferredTypeAndValue(res, Option(HList(0, 3))) shouldBe true
  }

  it should "find 1 index" in {
    val l = 2 :: List.range(0, 4)
    val res = l.indicesOfH(1, 2, 1)
    equalInferredTypeAndValue(res, Option(HList(3))) shouldBe true
  }

  it should "find 0 last indices" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfH(0, 1)
    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no last indices" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfH(2, 5)
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find not enough last indices" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfH(3, 1)
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: Int :: HNil]) shouldBe true
  }

  it should "find 2 last indices" in {
    val l = 2 :: List.range(0, 4)
    val res = l.lastIndicesOfH(2, 2)
    equalInferredTypeAndValue(res, Option(HList(3, 0))) shouldBe true
  }

  it should "find 1 last index" in {
    val l = 3 :: List.range(0, 4)
    val res = l.lastIndicesOfH(1, 3, 3)
    equalInferredTypeAndValue(res, Option(HList(0))) shouldBe true
  }

  it should "find 0 indices of slice" in {
    val l = List.range(0, 4)
    val res = l.indicesOfSliceH(0, List(1, 2))
    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no indices of slice" in {
    val l = List.range(0, 4)
    val res = l.indicesOfSliceH(2, List(1, 0))
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find not enough indices of slice" in {
    val l = List.range(0, 4)
    val res = l.indicesOfSliceH(2, List(1, 2))
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find 2 indices of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.indicesOfSliceH(2, List(1, 2))
    equalInferredTypeAndValue(res, Option(HList(1, 4))) shouldBe true
  }

  it should "find 1 index of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.indicesOfSliceH(1, List(1, 2), 2)
    equalInferredTypeAndValue(res, Option(HList(4))) shouldBe true
  }

  it should "find 0 last indices of slice" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfSliceH(0, List(0, 1))
    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no last indices of slice" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfSliceH(2, List(1, 0))
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find not enough last indices of slice" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfSliceH(2, List(1, 2))
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find 2 last indices of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.lastIndicesOfSliceH(2, List(1, 2))
    equalInferredTypeAndValue(res, Option(HList(4, 1))) shouldBe true
  }

  it should "find 1 last index of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.lastIndicesOfSliceH(1, List(1, 2), 3)
    equalInferredTypeAndValue(res, Option(HList(1))) shouldBe true
  }

}

class SeqLikeSizedExtensionsSpec extends FlatSpec with Matchers {

  "A SeqLike" should "not combine by 0" in {
    val l = List.range(0, 5)
    illTyped(""" l.combinationsS[List](0) """)
  }

  it should "combine by 1" in {
    val l = List.range(0, 5)
    val c = l.combinationsS(1)

    equalInferredType(c, List.empty[Sized[List[Int], nat._1]])

    c.lengthCompare(l.length) shouldBe 0
    c.toSet shouldBe l.map(Sized[List](_)).toSet
  }

  it should "combine by 2" in {
    val l = List.range(0, 4)
    val c = l.combinationsS(2)

    equalInferredType(c, List.empty[Sized[List[Int], nat._2]])

    c.lengthCompare(l.length * (l.length - 1) / 2) shouldBe 0
    c.toSet shouldBe Set(Sized[List](0, 1), Sized[List](0, 2), Sized[List](0, 3), Sized[List](1, 2), Sized[List](1, 3), Sized[List](2, 3))
  }

  it should "combine by 2 iterating" in {
    val l = List.range(0, 4)
    val c = l.combinationsIteratorS(2).toList

    equalInferredType(c, List.empty[Sized[List[Int], nat._2]])

    c.lengthCompare(l.length * (l.length - 1) / 2) shouldBe 0
    c.toSet shouldBe Set(Sized[List](0, 1), Sized[List](0, 2), Sized[List](0, 3), Sized[List](1, 2), Sized[List](1, 3), Sized[List](2, 3))
  }

  it should "find 0 indices" in {
    val l = List.range(0, 4)
    val res = l.indicesOfS(0, 1)
    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no indices" in {
    val l = List.range(0, 4)
    val res = l.indicesOfS(2, 5)
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find not enough indices" in {
    val l = List.range(0, 4)
    val res = l.indicesOfS(3, 1)
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._3]]) shouldBe true
  }

  it should "find 2 indices" in {
    val l = 2 :: List.range(0, 4)
    val res = l.indicesOfS(2, 2)
    equalInferredTypeAndValue(res, Option(Sized[List](0, 3))) shouldBe true
  }

  it should "find 1 index" in {
    val l = 2 :: List.range(0, 4)
    val res = l.indicesOfS(1, 2, 1)
    equalInferredTypeAndValue(res, Option(Sized[List](3))) shouldBe true
  }

  it should "find 0 last indices" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfS(0, 1)
    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no last indices" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfS(2, 5)
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find not enough last indices" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfS(3, 1)
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._3]]) shouldBe true
  }

  it should "find 2 last indices" in {
    val l = 2 :: List.range(0, 4)
    val res = l.lastIndicesOfS(2, 2)
    equalInferredTypeAndValue(res, Option(Sized[List](3, 0))) shouldBe true
  }

  it should "find 1 last index" in {
    val l = 3 :: List.range(0, 4)
    val res = l.lastIndicesOfS(1, 3, 3)
    equalInferredTypeAndValue(res, Option(Sized[List](0))) shouldBe true
  }

  it should "find 0 indices of slice" in {
    val l = List.range(0, 4)
    val res = l.indicesOfSliceS(0, List(1, 2))
    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no indices of slice" in {
    val l = List.range(0, 4)
    val res = l.indicesOfSliceS(2, List(1, 0))
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find not enough indices of slice" in {
    val l = List.range(0, 4)
    val res = l.indicesOfSliceS(2, List(1, 2))
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find 2 indices of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.indicesOfSliceS(2, List(1, 2))
    equalInferredTypeAndValue(res, Option(Sized[List](1, 4))) shouldBe true
  }

  it should "find 1 index of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.indicesOfSliceS(1, List(1, 2), 2)
    equalInferredTypeAndValue(res, Option(Sized[List](4))) shouldBe true
  }

  it should "find 0 last indices of slice" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfSliceS(0, List(0, 1))
    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no last indices of slice" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfSliceS(2, List(1, 0))
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find not enough last indices of slice" in {
    val l = List.range(0, 4)
    val res = l.lastIndicesOfSliceS(2, List(1, 2))
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find 2 last indices of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.lastIndicesOfSliceS(2, List(1, 2))
    equalInferredTypeAndValue(res, Option(Sized[List](4, 1))) shouldBe true
  }

  it should "find 1 last index of slice" in {
    val l = List.range(0, 3) ::: List.range(0, 4)
    val res = l.lastIndicesOfSliceS(1, List(1, 2), 3)
    equalInferredTypeAndValue(res, Option(Sized[List](1))) shouldBe true
  }

}
