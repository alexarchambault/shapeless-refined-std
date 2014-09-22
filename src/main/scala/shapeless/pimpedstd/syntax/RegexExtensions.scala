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

package shapeless.pimpedstd.syntax

import language.higherKinds
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import shapeless._
import shapeless.pimpedstd.ops.regex._


case class RegexTupleExtensions(regex: Regex) {

  def findInT(n: Nat, source: CharSequence) (implicit findIn: FindSubStringsIn[n.N, Product]): findIn.Out = 
    findIn(regex, source)

  def findMatchesInT(n: Nat, source: CharSequence) 
  (implicit findMatchesIn: FindMatchesIn[n.N, Product]): findMatchesIn.Out =
    findMatchesIn(regex, source)

  def splitT(n: Nat, toSplit: CharSequence) (implicit split: Split[n.N, Product]): split.Out = 
    split(regex, toSplit)
  
}

case class RegexHListExtensions(regex: Regex) {

  def findInH(n: Nat, source: CharSequence) (implicit findIn: FindSubStringsIn[n.N, HList]): findIn.Out =
    findIn(regex, source)

  def findMatchesInH(n: Nat, source: CharSequence) 
  (implicit findMatchesIn: FindMatchesIn[n.N, HList]): findMatchesIn.Out =
    findMatchesIn(regex, source)

  def splitH(n: Nat, toSplit: CharSequence) (implicit split: Split[n.N, HList]): split.Out =
    split(regex, toSplit)

}

case class RegexSizedExtensions(regex: Regex) {

  def findInS[CC[_]](n: Nat, source: CharSequence) 
  (implicit findIn: FindSubStringsIn[n.N, Sized[CC[String], n.N]]): findIn.Out =
    findIn(regex, source)

  def findMatchesInS[CC[_]](n: Nat, source: CharSequence) 
  (implicit findMatchesIn: FindMatchesIn[n.N, Sized[CC[Match], n.N]]): findMatchesIn.Out =
    findMatchesIn(regex, source)

  def splitS[CC[_]](n: Nat, toSplit: CharSequence) (implicit split: Split[n.N, Sized[CC[String], n.N]]): split.Out =
    split(regex, toSplit)

}