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

import shapeless._
import shapeless.test.illTyped
import shapeless.syntax.std.tuple._
import shapeless.pimpedstd.syntax._
import org.scalatest._
import shapeless.pimpedstd.test._

class StringTupleExtensionsSpec extends FlatSpec with Matchers {
  
  "A string" should "return an empty index set" in {
    val s = "aaa"
    val res = s.indicesOfT(0, "aa")
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }
  
  it should "find no indices" in {
    val s = "abcdd"
    val res = s.indicesOfT(2, "ef")
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find three indices" in {
    val s = "Aaa bb cccc d aa ffff aaa"
    val res = s.indicesOfT(3, "aa")
    equalInferredTypeAndValue(res, Option((1, 14, 22))) shouldBe true
  }

  it should "not find enough indices" in {
    val s = "Aaa bb cccc d aa ffff aa"
    val res = s.indicesOfT(3, "aa", 2)
    equalInferredTypeAndValue(res, Option.empty[(Int, Int, Int)]) shouldBe true
  }

  it should "return an empty char-index set" in {
    val s = "aaa"
    val res = s.indicesOfT(0, 'a')
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }

  it should "find no char-indices" in {
    val s = "abcdd"
    val res = s.indicesOfT(2, 'f')
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find three char-indices" in {
    val s = "Aaa bb cccc d aa ffff aaa"
    val res = s.indicesOfT(3, 'a')
    equalInferredTypeAndValue(res, Option((1, 2, 14))) shouldBe true
  }

  it should "not find enough char-indices" in {
    val s = "Aaa bb cccc d a ffff"
    val res = s.indicesOfT(3, 'a', 2)
    equalInferredTypeAndValue(res, Option.empty[(Int, Int, Int)]) shouldBe true
  }

  it should "return an empty last index set" in {
    val s = "aaa"
    val res = s.lastIndicesOfT(0, "aa")
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }

  it should "find no last indices" in {
    val s = "abcdd"
    val res = s.lastIndicesOfT(2, "ef")
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find three last indices" in {
    val s = "Aaa bb cccc d aa ffff aaa"
    val res = s.lastIndicesOfT(3, "aa")
    equalInferredTypeAndValue(res, Option((23, 22, 14))) shouldBe true
  }

  it should "not find enough last indices" in {
    val s = "Aaa bb cccc d aa ffff aa"
    val res = s.lastIndicesOfT(3, "aa", 21)
    equalInferredTypeAndValue(res, Option.empty[(Int, Int, Int)]) shouldBe true
  }

  it should "return an empty last char-index set" in {
    val s = "aaa"
    val res = s.lastIndicesOfT(0, 'a')
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }

  it should "find no last char-indices" in {
    val s = "abcdd"
    val res = s.lastIndicesOfT(2, 'f')
    equalInferredTypeAndValue(res, Option.empty[(Int, Int)]) shouldBe true
  }

  it should "find three last char-indices" in {
    val s = "Aaa bb cccc d aa ffff aa"
    val res = s.lastIndicesOfT(3, 'a')
    equalInferredTypeAndValue(res, Option((23, 22, 15))) shouldBe true
  }

  it should "not find enough last char-indices" in {
    val s = "Aaa bb cccc d a ffff"
    val res = s.lastIndicesOfT(3, 'f', 17)
    equalInferredTypeAndValue(res, Option.empty[(Int, Int, Int)]) shouldBe true
  }
  
  it should "refuse to split in 0 parts" in {
    val s = "first;second"
    illTyped(""" s.splitT(0, ";") """) 
  }

  it should "split in 1 part" in {
    val s = "first;second"
    val res = s.splitT(1, ";")
    equalInferredTypeAndValue(res, Option(Tuple1(s))) shouldBe true
  }

  it should "split in 3 parts" in {
    val s = "first;second;third;fourth"
    val res = s.splitT(3, ";")
    equalInferredTypeAndValue(res, Option(("first", "second", "third;fourth"))) shouldBe true
  }

  it should "not split in 3 parts" in {
    val s = "first;second"
    val res = s.splitT(3, ";")
    equalInferredTypeAndValue(res, Option.empty[(String, String, String)]) shouldBe true
  }

  it should "refuse to split with char in 0 parts" in {
    val s = "first;second"
    illTyped(""" s.splitT(0, ';') """)
  }

  it should "split with char in 1 part" in {
    val s = "first;second"
    val res = s.splitT(1, ';')
    equalInferredTypeAndValue(res, Option(Tuple1(s))) shouldBe true
  }

  it should "split with char in 3 parts" in {
    val s = "first;second;third;fourth"
    val res = s.splitT(3, ';')
    equalInferredTypeAndValue(res, Option(("first", "second", "third;fourth"))) shouldBe true
  }

  it should "not split with char in 3 parts" in {
    val s = "first;second"
    val res = s.splitT(3, ';')
    equalInferredTypeAndValue(res, Option.empty[(String, String, String)]) shouldBe true
  }

  it should "refuse to split with a sequence of char in 0 parts" in {
    val s = "first;second"
    illTyped(""" s.splitT(0, Seq(';', ':')) """)
  }

  it should "split with a sequence of char in 1 part" in {
    val s = "first;second"
    val res = s.splitT(1, Seq(';', ':'))
    equalInferredTypeAndValue(res, Option(Tuple1(s))) shouldBe true
  }

  it should "split with a sequence of char in 3 parts" in {
    val s = "first;second:third;fourth"
    val res = s.splitT(3, Seq(';', ':'))
    equalInferredTypeAndValue(res, Option(("first", "second", "third;fourth"))) shouldBe true
  }

  it should "not split with a sequence of char in 3 parts" in {
    val s = "first;second"
    val res = s.splitT(3, Seq(';', ':'))
    equalInferredTypeAndValue(res, Option.empty[(String, String, String)]) shouldBe true
  }

}

class StringHListExtensionsSpec extends FlatSpec with Matchers {

  "A string" should "return an empty index set" in {
    val s = "aaa"
    val res = s.indicesOfH(0, "aa")
    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no indices" in {
    val s = "abcdd"
    val res = s.indicesOfH(2, "ef")
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find three indices" in {
    val s = "Aaa bb cccc d aa ffff aaa"
    val res = s.indicesOfH(3, "aa")
    equalInferredTypeAndValue(res, Option(HList(1, 14, 22))) shouldBe true
  }

  it should "not find enough indices" in {
    val s = "Aaa bb cccc d aa ffff aa"
    val res = s.indicesOfH(3, "aa", 2)
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: Int :: HNil]) shouldBe true
  }

  it should "return an empty char-index set" in {
    val s = "aaa"
    val res = s.indicesOfH(0, 'a')
    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no char-indices" in {
    val s = "abcdd"
    val res = s.indicesOfH(2, 'f')
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find three char-indices" in {
    val s = "Aaa bb cccc d aa ffff aaa"
    val res = s.indicesOfH(3, 'a')
    equalInferredTypeAndValue(res, Option(HList(1, 2, 14))) shouldBe true
  }

  it should "not find enough char-indices" in {
    val s = "Aaa bb cccc d a ffff"
    val res = s.indicesOfH(3, 'a', 2)
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: Int :: HNil]) shouldBe true
  }

  it should "return an empty last index set" in {
    val s = "aaa"
    val res = s.lastIndicesOfH(0, "aa")
    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no last indices" in {
    val s = "abcdd"
    val res = s.lastIndicesOfH(2, "ef")
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find three last indices" in {
    val s = "Aaa bb cccc d aa ffff aaa"
    val res = s.lastIndicesOfH(3, "aa")
    equalInferredTypeAndValue(res, Option(HList(23, 22, 14))) shouldBe true
  }

  it should "not find enough last indices" in {
    val s = "Aaa bb cccc d aa ffff aa"
    val res = s.lastIndicesOfH(3, "aa", 21)
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: Int :: HNil]) shouldBe true
  }

  it should "return an empty last char-index set" in {
    val s = "aaa"
    val res = s.lastIndicesOfH(0, 'a')
    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find no last char-indices" in {
    val s = "abcdd"
    val res = s.lastIndicesOfH(2, 'f')
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: HNil]) shouldBe true
  }

  it should "find three last char-indices" in {
    val s = "Aaa bb cccc d aa ffff aa"
    val res = s.lastIndicesOfH(3, 'a')
    equalInferredTypeAndValue(res, Option(HList(23, 22, 15))) shouldBe true
  }

  it should "not find enough last char-indices" in {
    val s = "Aaa bb cccc d a ffff"
    val res = s.lastIndicesOfH(3, 'f', 17)
    equalInferredTypeAndValue(res, Option.empty[Int :: Int :: Int :: HNil]) shouldBe true
  }

  it should "refuse to split in 0 parts" in {
    val s = "first;second"
    illTyped(""" s.splitH(0, ";") """)
  }

  it should "split in 1 part" in {
    val s = "first;second"
    val res = s.splitH(1, ";")
    equalInferredTypeAndValue(res, Option(HList(s))) shouldBe true
  }

  it should "split in 3 parts" in {
    val s = "first;second;third;fourth"
    val res = s.splitH(3, ";")
    equalInferredTypeAndValue(res, Option(HList("first", "second", "third;fourth"))) shouldBe true
  }

  it should "not split in 3 parts" in {
    val s = "first;second"
    val res = s.splitH(3, ";")
    equalInferredTypeAndValue(res, Option.empty[String :: String :: String :: HNil]) shouldBe true
  }

  it should "refuse to split with char in 0 parts" in {
    val s = "first;second"
    illTyped(""" s.splitH(0, ';') """)
  }

  it should "split with char in 1 part" in {
    val s = "first;second"
    val res = s.splitH(1, ';')
    equalInferredTypeAndValue(res, Option(HList(s))) shouldBe true
  }

  it should "split with char in 3 parts" in {
    val s = "first;second;third;fourth"
    val res = s.splitH(3, ';')
    equalInferredTypeAndValue(res, Option(HList("first", "second", "third;fourth"))) shouldBe true
  }

  it should "not split with char in 3 parts" in {
    val s = "first;second"
    val res = s.splitH(3, ';')
    equalInferredTypeAndValue(res, Option.empty[String :: String :: String :: HNil]) shouldBe true
  }

  it should "refuse to split with a sequence of char in 0 parts" in {
    val s = "first;second"
    illTyped(""" s.splitH(0, Seq(';', ':')) """)
  }

  it should "split with a sequence of char in 1 part" in {
    val s = "first;second"
    val res = s.splitH(1, Seq(';', ':'))
    equalInferredTypeAndValue(res, Option(HList(s))) shouldBe true
  }

  it should "split with a sequence of char in 3 parts" in {
    val s = "first;second:third;fourth"
    val res = s.splitH(3, Seq(';', ':'))
    equalInferredTypeAndValue(res, Option(HList("first", "second", "third;fourth"))) shouldBe true
  }

  it should "not split with a sequence of char in 3 parts" in {
    val s = "first;second"
    val res = s.splitH(3, Seq(';', ':'))
    equalInferredTypeAndValue(res, Option.empty[String :: String :: String :: HNil]) shouldBe true
  }

}

class StringSizedExtensionsSpec extends FlatSpec with Matchers {

  "A string" should "return an empty index set" in {
    val s = "aaa"
    val res = s.indicesOfS[List](0, "aa")
    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no indices" in {
    val s = "abcdd"
    val res = s.indicesOfS[List](2, "ef")
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find three indices" in {
    val s = "Aaa bb cccc d aa ffff aaa"
    val res = s.indicesOfS[List](3, "aa")
    equalInferredTypeAndValue(res, Option(Sized[List](1, 14, 22))) shouldBe true
  }

  it should "not find enough indices" in {
    val s = "Aaa bb cccc d aa ffff aa"
    val res = s.indicesOfS[List](3, "aa", 2)
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._3]]) shouldBe true
  }

  it should "return an empty char-index set" in {
    val s = "aaa"
    val res = s.indicesOfS[List](0, 'a')
    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no char-indices" in {
    val s = "abcdd"
    val res = s.indicesOfS[List](2, 'f')
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find three char-indices" in {
    val s = "Aaa bb cccc d aa ffff aaa"
    val res = s.indicesOfS[List](3, 'a')
    equalInferredTypeAndValue(res, Option(Sized[List](1, 2, 14))) shouldBe true
  }

  it should "not find enough char-indices" in {
    val s = "Aaa bb cccc d a ffff"
    val res = s.indicesOfS[List](3, 'a', 2)
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._3]]) shouldBe true
  }

  it should "return an empty last index set" in {
    val s = "aaa"
    val res = s.lastIndicesOfS[List](0, "aa")
    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no last indices" in {
    val s = "abcdd"
    val res = s.lastIndicesOfS[List](2, "ef")
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find three last indices" in {
    val s = "Aaa bb cccc d aa ffff aaa"
    val res = s.lastIndicesOfS[List](3, "aa")
    equalInferredTypeAndValue(res, Option(Sized[List](23, 22, 14))) shouldBe true
  }

  it should "not find enough last indices" in {
    val s = "Aaa bb cccc d aa ffff aa"
    val res = s.lastIndicesOfS[List](3, "aa", 21)
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._3]]) shouldBe true
  }

  it should "return an empty last char-index set" in {
    val s = "aaa"
    val res = s.lastIndicesOfS[List](0, 'a')
    equalInferredTypeAndValue(res, Option(Sized.empty[List[Int]])) shouldBe true
  }

  it should "find no last char-indices" in {
    val s = "abcdd"
    val res = s.lastIndicesOfS[List](2, 'f')
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._2]]) shouldBe true
  }

  it should "find three last char-indices" in {
    val s = "Aaa bb cccc d aa ffff aa"
    val res = s.lastIndicesOfS[List](3, 'a')
    equalInferredTypeAndValue(res, Option(Sized[List](23, 22, 15))) shouldBe true
  }

  it should "not find enough last char-indices" in {
    val s = "Aaa bb cccc d a ffff"
    val res = s.lastIndicesOfS[List](3, 'f', 17)
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Int], nat._3]]) shouldBe true
  }

  it should "refuse to split in 0 parts" in {
    val s = "first;second"
    illTyped(""" s.splitS[List](0, ";") """)
  }

  it should "split in 1 part" in {
    val s = "first;second"
    val res = s.splitS[List](1, ";")
    equalInferredTypeAndValue(res, Option(Sized[List](s))) shouldBe true
  }

  it should "split in 3 parts" in {
    val s = "first;second;third;fourth"
    val res = s.splitS[List](3, ";")
    equalInferredTypeAndValue(res, Option(Sized[List]("first", "second", "third;fourth"))) shouldBe true
  }

  it should "not split in 3 parts" in {
    val s = "first;second"
    val res = s.splitS[List](3, ";")
    equalInferredTypeAndValue(res, Option.empty[Sized[List[String], nat._3]]) shouldBe true
  }

  it should "refuse to split with char in 0 parts" in {
    val s = "first;second"
    illTyped(""" s.splitS[List](0, ';') """)
  }

  it should "split with char in 1 part" in {
    val s = "first;second"
    val res = s.splitS[List](1, ';')
    equalInferredTypeAndValue(res, Option(Sized[List](s))) shouldBe true
  }

  it should "split with char in 3 parts" in {
    val s = "first;second;third;fourth"
    val res = s.splitS[List](3, ';')
    equalInferredTypeAndValue(res, Option(Sized[List]("first", "second", "third;fourth"))) shouldBe true
  }

  it should "not split with char in 3 parts" in {
    val s = "first;second"
    val res = s.splitS[List](3, ';')
    equalInferredTypeAndValue(res, Option.empty[Sized[List[String], nat._3]]) shouldBe true
  }

  it should "refuse to split with a sequence of char in 0 parts" in {
    val s = "first;second"
    illTyped(""" s.splitS[List](0, Seq(';', ':')) """)
  }

  it should "split with a sequence of char in 1 part" in {
    val s = "first;second"
    val res = s.splitS[List](1, Seq(';', ':'))
    equalInferredTypeAndValue(res, Option(Sized[List](s))) shouldBe true
  }

  it should "split with a sequence of char in 3 parts" in {
    val s = "first;second:third;fourth"
    val res = s.splitS[List](3, Seq(';', ':'))
    equalInferredTypeAndValue(res, Option(Sized[List]("first", "second", "third;fourth"))) shouldBe true
  }

  it should "not split with a sequence of char in 3 parts" in {
    val s = "first;second"
    val res = s.splitS[List](3, Seq(';', ':'))
    equalInferredTypeAndValue(res, Option.empty[Sized[List[String], nat._3]]) shouldBe true
  }

}
