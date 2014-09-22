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

import scala.util.matching.Regex
import shapeless._
import shapeless.syntax.std.tuple._
import shapeless.pimpedstd.syntax._
import org.scalatest._
import shapeless.pimpedstd.test._

object matchUnderlying extends Poly1 {
  implicit val regexMatch = at[Regex.Match](_.matched)
}

class RegexTupleExtensionsSpec extends FlatSpec with Matchers {

  "A regex" should "find an empty result set" in {
    val r = "a".r
    val res = r.findInT(0, "abb")
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }
  
  it should "find none" in {
    val r = "a".r
    val res = r.findInT(2, "bbb")
    equalInferredTypeAndValue(res, Option.empty[(String, String)]) shouldBe true
  }

  it should "find three" in {
    val r = "[ab]".r
    val res = r.findInT(3, "a c ba")
    equalInferredTypeAndValue(res, Option(("a", "b", "a"))) shouldBe true
  }

  it should "find an empty result set of matches" in {
    val r = "a".r
    val res = r.findMatchesInT(0, "abb")
    equalInferredTypeAndValue(res, Option(()))
  }

  it should "find no match" in {
    val r = "a".r
    val res = r.findMatchesInT(2, "bbb")
    equalInferredTypeAndValue(res, Option.empty[(Regex.Match, Regex.Match)]) shouldBe true
  }
  
  it should "find three matches" in {
    val r = "[ab]".r
    val res = r.findMatchesInT(3, "a c ba")
    
    equalInferredTypeAndValue(res.map(_ map matchUnderlying), Option(("a", "b", "a"))) shouldBe true
  }
  
  it should "split in an empty result set" in {
    val r = "a".r
    val res = r.splitT(0, "abb")
    
    equalInferredTypeAndValue(res, Option(())) shouldBe true
  }
  
  it should "not split" in {
    val r = "a".r
    val res = r.splitT(3, "cabb")

    equalInferredTypeAndValue(res, Option.empty[(String, String, String)]) shouldBe true
  }

  it should "split in 4" in {
    val r = "[ab]".r
    val res = r.splitT(4, "ca0bbb")

    equalInferredTypeAndValue(res, Option(("c", "0", "", "b"))) shouldBe true
  }

}

class RegexHListExtensionsSpec extends FlatSpec with Matchers {

  "A regex" should "find an empty result set" in {
    val r = "a".r
    val res = r.findInH(0, "abb")
    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "find none" in {
    val r = "a".r
    val res = r.findInH(2, "bbb")
    equalInferredTypeAndValue(res, Option.empty[String :: String :: HNil]) shouldBe true
  }

  it should "find three" in {
    val r = "[ab]".r
    val res = r.findInH(3, "a c ba")
    equalInferredTypeAndValue(res, Option(HList("a", "b", "a"))) shouldBe true
  }

  it should "find an empty result set of matches" in {
    val r = "a".r
    val res = r.findMatchesInH(0, "abb")
    equalInferredTypeAndValue(res, Option(HNil: HNil))
  }

  it should "find no match" in {
    val r = "a".r
    val res = r.findMatchesInH(2, "bbb")
    equalInferredTypeAndValue(res, Option.empty[Regex.Match :: Regex.Match :: HNil]) shouldBe true
  }

  it should "find three matches" in {
    val r = "[ab]".r
    val res = r.findMatchesInH(3, "a c ba")

    equalInferredTypeAndValue(res.map(_ map matchUnderlying), Option(HList("a", "b", "a"))) shouldBe true
  }

  it should "split in an empty result set" in {
    val r = "a".r
    val res = r.splitH(0, "abb")

    equalInferredTypeAndValue(res, Option(HNil: HNil)) shouldBe true
  }

  it should "not split" in {
    val r = "a".r
    val res = r.splitH(3, "cabb")

    equalInferredTypeAndValue(res, Option.empty[String :: String :: String :: HNil]) shouldBe true
  }

  it should "split in 4" in {
    val r = "[ab]".r
    val res = r.splitH(4, "ca0bbb")

    equalInferredTypeAndValue(res, Option(HList("c", "0", "", "b"))) shouldBe true
  }

}

class RegexSizedExtensionsSpec extends FlatSpec with Matchers {

  "A regex" should "find an empty result set" in {
    val r = "a".r
    val res = r.findInS[List](0, "abb")
    equalInferredTypeAndValue(res, Option(Sized.empty[List[String]])) shouldBe true
  }

  it should "find none" in {
    val r = "a".r
    val res = r.findInS[List](2, "bbb")
    equalInferredTypeAndValue(res, Option.empty[Sized[List[String], nat._2]]) shouldBe true
  }

  it should "find three" in {
    val r = "[ab]".r
    val res = r.findInS[List](3, "a c ba")
    equalInferredTypeAndValue(res, Option(Sized[List]("a", "b", "a"))) shouldBe true
  }

  it should "find an empty result set of matches" in {
    val r = "a".r
    val res = r.findMatchesInS[List](0, "abb")
    equalInferredTypeAndValue(res, Option(Sized.empty[List[Regex.Match]]))
  }

  it should "find no match" in {
    val r = "a".r
    val res = r.findMatchesInS[List](2, "bbb")
    equalInferredTypeAndValue(res, Option.empty[Sized[List[Regex.Match], nat._2]]) shouldBe true
  }

  it should "find three matches" in {
    val r = "[ab]".r
    val res = r.findMatchesInS[List](3, "a c ba")

    equalInferredTypeAndValue(res.map(_ map matchUnderlying), Option(Sized[List]("a", "b", "a"))) shouldBe true
  }

  it should "split in an empty result set" in {
    val r = "a".r
    val res = r.splitS[List](0, "abb")

    equalInferredTypeAndValue(res, Option(Sized.empty[List[String]])) shouldBe true
  }

  it should "not split" in {
    val r = "a".r
    val res = r.splitS[List](3, "cabb")

    equalInferredTypeAndValue(res, Option.empty[Sized[List[String], nat._3]]) shouldBe true
  }

  it should "split in 4" in {
    val r = "[ab]".r
    val res = r.splitS[List](4, "ca0bbb")

    equalInferredTypeAndValue(res, Option(Sized[List]("c", "0", "", "b"))) shouldBe true
  }

}
