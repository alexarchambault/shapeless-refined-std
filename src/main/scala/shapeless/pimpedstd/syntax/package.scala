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

import shapeless.pimpedstd.util.IsSeqLikeAux

import language.implicitConversions
import shapeless.{ Sized, _0 }
import scala.collection.generic.IsSeqLike
import scala.util.matching.Regex

trait LowPrioritySyntaxImplicits {
  import syntax._
  
  implicit def seqLikeTupleExtensions[Repr](repr: Repr) (implicit ev: IsSeqLike[Repr]) =
    SeqLikeTupleExtensions(repr, ev.conversion)
  implicit def seqLikeHListExtensions[Repr](repr: Repr) (implicit ev: IsSeqLike[Repr]) =
    SeqLikeHListExtensions(repr, ev.conversion)
  implicit def seqLikeSizedExtensions[Repr](repr: Repr) (implicit ev: IsSeqLike[Repr]) =
    SeqLikeSizedExtensions(repr, ev.conversion)

}

package object syntax extends LowPrioritySyntaxImplicits {

  implicit def traversableOnceTupleExtensions[A](t: TraversableOnce[A]) =
    TraversableOnceTupleExtensions(t)
  implicit def traversableOnceHListExtensions[A](t: TraversableOnce[A]) =
    TraversableOnceHListExtensions(t)
  implicit def traversableOnceSizedExtensions[A, Repr](t: Repr) (implicit ev: Repr <:< TraversableOnce[A]) =
    TraversableOnceSizedExtensions[A, Repr](t)
  implicit def traversableOnceExtraExtensions[A](t: TraversableOnce[A]) =
    TraversableOnceExtraExtensions(t)

  implicit def traversableLikeTupleExtensions[Repr](repr: Repr) (implicit ev: IsTraversableLike[Repr]) =
    TraversableLikeTupleExtensions(repr, ev.conversion)
  implicit def traversableLikeHListExtensions[Repr](repr: Repr) (implicit ev: IsTraversableLike[Repr]) =
    TraversableLikeHListExtensions(repr, ev.conversion)
  implicit def traversableLikeSizedExtensions[Repr](repr: Repr) (implicit ev: IsTraversableLike[Repr]) =
    TraversableLikeSizedExtensions(repr, ev.conversion)
  implicit def traversableLikeExtraExtensions[Repr](repr: Repr) (implicit ev: IsTraversableLike[Repr]) =
    TraversableLikeExtraExtensions(repr, ev.conversion)

  implicit def iterableLikeTupleExtensions[Repr](repr: Repr) (implicit ev: IsIterableLike[Repr]) =
    IterableLikeTupleExtensions(repr, ev.conversion)
  implicit def iterableLikeHListExtensions[Repr](repr: Repr) (implicit ev: IsIterableLike[Repr]) =
    IterableLikeHListExtensions(repr, ev.conversion)
  implicit def iterableLikeSizedExtensions[Repr](repr: Repr) (implicit ev: IsIterableLike[Repr]) =
    IterableLikeSizedExtensions(repr, ev.conversion)
  implicit def iterableLikeExtraExtensions[Repr](repr: Repr) (implicit ev: IsIterableLike[Repr]) =
    IterableLikeExtraExtensions(repr, ev.conversion)

  implicit def iteratorTupleExtensions[A](it: Iterator[A]) = IteratorTupleExtensions(it)
  implicit def iteratorHListExtensions[A](it: Iterator[A]) = IteratorHListExtensions(it)
  implicit def iteratorSizedExtensions[A](it: Iterator[A]) = IteratorSizedExtensions(it)
  implicit def iteratorExtraExtensions[A](it: Iterator[A]) = IteratorExtraExtensions(it)

  implicit def stringTupleExtensions[Repr](str: Repr) (implicit ev: IsSeqLikeAux[Repr, Char]) = 
    StringTupleExtensions(str, ev.conversion)
  implicit def stringHListExtensions[Repr](str: Repr) (implicit ev: IsSeqLikeAux[Repr, Char]) = 
    StringHListExtensions(str, ev.conversion)
  implicit def stringSizedExtensions[Repr](str: Repr) (implicit ev: IsSeqLikeAux[Repr, Char]) = 
    StringSizedExtensions(str, ev.conversion)

  implicit def regexTupleExtensions(regex: Regex) = RegexTupleExtensions(regex)
  implicit def regexHListExtensions(regex: Regex) = RegexHListExtensions(regex)
  implicit def regexSizedExtensions(regex: Regex) = RegexSizedExtensions(regex)


  implicit class IteratorSingletonExtensions(val self: Iterator.type) extends AnyVal {
    import shapeless.pimpedstd.ops.iterator.Zip

    def zip[L](items: L) (implicit zip: Zip[L]): zip.Out = zip(items)
  }

  implicit class IterableSingletonExtensions(val self: Iterable.type) extends AnyVal {
    import shapeless.pimpedstd.ops.iterablelike.Zip

    def zip[L](items: L) (implicit zip: Zip[L]): zip.Out = zip(items)
  }

  implicit class SizedSingletonExtensions(val self: Sized.type) extends AnyVal {
    import shapeless.pimpedstd.ops.sized.Empty

    def empty[Repr] (implicit empty: Empty[Repr]): Sized[Repr, _0] = empty()
  }

}
