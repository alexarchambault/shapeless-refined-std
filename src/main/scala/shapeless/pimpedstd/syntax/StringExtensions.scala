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
import scala.collection.SeqLike
import shapeless.{Sized, HList, Nat}
import shapeless.pimpedstd.ops.string._


/*
 * Converting coll (type Repr) to a String by calling .toString on it,
 * like it is done in StringLike (https://github.com/scala/scala/blob/2.12.x/src/library/scala/collection/immutable/StringLike.scala#L201)
 */

/*
 * Maybe type Repr should be used in some return types, instead of String ? (So that StringBuilder would remain StringBuilders, ...)
 */

private object Util {

  // From scala.collection.immutable.StringLike
  def escape(ch: Char): String = "\\Q" + ch + "\\E"

}

case class StringTupleExtensions[Repr](coll: Repr, toCollection: Repr => SeqLike[Char, Repr]) {

  def indicesOfT(count: Nat, s: String) (implicit indicesOf: IndicesOf[count.N, Product]): indicesOf.Out = 
    indicesOf(coll.toString, s, 0)
  def indicesOfT(count: Nat, s: String, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, Product]): indicesOf.Out = 
    indicesOf(coll.toString, s, fromIndex)
  
  def indicesOfT(count: Nat, ch: Int) (implicit indicesOf: IndicesOf[count.N, Product]): indicesOf.Out = 
    indicesOf.char(coll.toString, ch, 0)  
  def indicesOfT(count: Nat, ch: Int, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, Product]): indicesOf.Out =
    indicesOf.char(coll.toString, ch, fromIndex)

  def lastIndicesOfT(count: Nat, s: String) (implicit indicesOf: IndicesOf[count.N, Product]): indicesOf.Out = {
    val str = coll.toString
    indicesOf.last(str, s, str.length)
  }
  def lastIndicesOfT(count: Nat, s: String, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, Product]): indicesOf.Out =
    indicesOf.last(coll.toString, s, fromIndex)
  
  def lastIndicesOfT(count: Nat, ch: Int) (implicit indicesOf: IndicesOf[count.N, Product]): indicesOf.Out = {
    val str = coll.toString
    indicesOf.last(str, ch, str.length)
  }
  def lastIndicesOfT(count: Nat, ch: Int, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, Product]): indicesOf.Out = 
    indicesOf.last(coll.toString, ch, fromIndex)


  def splitT(n: Nat, regex: String) (implicit split: Split[n.N, Product]): split.Out = 
    split(coll.toString, regex)
  def splitT(n: Nat, separator: Char) (implicit split: Split[n.N, Product]): split.Out = 
    split(coll.toString, Util.escape(separator))
  def splitT(n: Nat, separators: Seq[Char]) (implicit split: Split[n.N, Product]): split.Out = 
    split(coll.toString, separators.foldLeft("[")(_+Util.escape(_)) + "]")
  
}

case class StringHListExtensions[Repr](coll: Repr, toCollection: Repr => SeqLike[Char, Repr]) {

  def indicesOfH(count: Nat, s: String) (implicit indicesOf: IndicesOf[count.N, HList]): indicesOf.Out =
    indicesOf(coll.toString, s, 0)
  def indicesOfH(count: Nat, s: String, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, HList]): indicesOf.Out =
    indicesOf(coll.toString, s, fromIndex)

  def indicesOfH(count: Nat, ch: Int) (implicit indicesOf: IndicesOf[count.N, HList]): indicesOf.Out =
    indicesOf.char(coll.toString, ch, 0)
  def indicesOfH(count: Nat, ch: Int, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, HList]): indicesOf.Out =
    indicesOf.char(coll.toString, ch, fromIndex)

  def lastIndicesOfH(count: Nat, s: String) (implicit indicesOf: IndicesOf[count.N, HList]): indicesOf.Out = {
    val str = coll.toString
    indicesOf.last(str, s, str.length)
  }
  def lastIndicesOfH(count: Nat, s: String, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, HList]): indicesOf.Out =
    indicesOf.last(coll.toString, s, fromIndex)

  def lastIndicesOfH(count: Nat, ch: Int) (implicit indicesOf: IndicesOf[count.N, HList]): indicesOf.Out = {
    val str = coll.toString
    indicesOf.last(str, ch, str.length)
  }
  def lastIndicesOfH(count: Nat, ch: Int, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, HList]): indicesOf.Out =
    indicesOf.last(coll.toString, ch, fromIndex)


  def splitH(n: Nat, regex: String) (implicit split: Split[n.N, HList]): split.Out =
    split(coll.toString, regex)
  def splitH(n: Nat, separator: Char) (implicit split: Split[n.N, HList]): split.Out =
    split(coll.toString, Util.escape(separator))
  def splitH(n: Nat, separators: Seq[Char]) (implicit split: Split[n.N, HList]): split.Out =
    split(coll.toString, separators.foldLeft("[")(_+Util.escape(_)) + "]")

}

case class StringSizedExtensions[Repr](coll: Repr, toCollection: Repr => SeqLike[Char, Repr]) {

  def indicesOfS[CC[_]](count: Nat, s: String) (implicit indicesOf: IndicesOf[count.N, Sized[CC[Int], count.N]]): indicesOf.Out =
    indicesOf(coll.toString, s, 0)
  def indicesOfS[CC[_]](count: Nat, s: String, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, Sized[CC[Int], count.N]]): indicesOf.Out =
    indicesOf(coll.toString, s, fromIndex)

  def indicesOfS[CC[_]](count: Nat, ch: Int) (implicit indicesOf: IndicesOf[count.N, Sized[CC[Int], count.N]]): indicesOf.Out =
    indicesOf.char(coll.toString, ch, 0)
  def indicesOfS[CC[_]](count: Nat, ch: Int, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, Sized[CC[Int], count.N]]): indicesOf.Out =
    indicesOf.char(coll.toString, ch, fromIndex)

  def lastIndicesOfS[CC[_]](count: Nat, s: String) (implicit indicesOf: IndicesOf[count.N, Sized[CC[Int], count.N]]): indicesOf.Out = {
    val str = coll.toString
    indicesOf.last(str, s, str.length)
  }
  def lastIndicesOfS[CC[_]](count: Nat, s: String, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, Sized[CC[Int], count.N]]): indicesOf.Out =
    indicesOf.last(coll.toString, s, fromIndex)

  def lastIndicesOfS[CC[_]](count: Nat, ch: Int) (implicit indicesOf: IndicesOf[count.N, Sized[CC[Int], count.N]]): indicesOf.Out = {
    val str = coll.toString
    indicesOf.last(str, ch, str.length)
  }
  def lastIndicesOfS[CC[_]](count: Nat, ch: Int, fromIndex: Int) (implicit indicesOf: IndicesOf[count.N, Sized[CC[Int], count.N]]): indicesOf.Out =
    indicesOf.last(coll.toString, ch, fromIndex)


  def splitS[CC[_]](n: Nat, regex: String) (implicit split: Split[n.N, Sized[CC[String], n.N]]): split.Out =
    split(coll.toString, regex)
  def splitS[CC[_]](n: Nat, separator: Char) (implicit split: Split[n.N, Sized[CC[String], n.N]]): split.Out =
    split(coll.toString, Util.escape(separator))
  def splitS[CC[_]](n: Nat, separators: Seq[Char]) (implicit split: Split[n.N, Sized[CC[String], n.N]]): split.Out =
    split(coll.toString, separators.foldLeft("[")(_+Util.escape(_)) + "]")

}