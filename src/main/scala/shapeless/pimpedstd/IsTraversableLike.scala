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

import scala.language.higherKinds
import scala.collection.TraversableLike

/**
 * Evidence that `Repr` can be viewed as a `TraversableLike[Repr]`.
 * 
 * Similar to `scala.collection.generic.IsTraversableLike`, but 
 * for `TraversableLike` and instead of `GenTraversableLike`,
 * `TraversableLike` having more methods than `GenTraversableLike` 
 * (`toList`, ...).
 */
trait IsTraversableLike[Repr] {
  type A
  val conversion: Repr => TraversableLike[A, Repr]
}

object IsTraversableLike {
  type Aux[Repr, A0] = IsTraversableLike[Repr] { type A = A0 }
  
  def apply[Repr] (implicit 
    isTraversableLike: IsTraversableLike[Repr]
  ): Aux[Repr, isTraversableLike.A] = isTraversableLike

  implicit val stringRepr: Aux[String, Char] =
    new IsTraversableLike[String] {
      type A = Char
      val conversion = implicitly[String => TraversableLike[Char, String]]
    }

  implicit def genTraversableLikeRepr[C[_], A0] (implicit 
    conv: C[A0] => TraversableLike[A0, C[A0]]
  ): Aux[C[A0], A0] =
    new IsTraversableLike[C[A0]] {
      type A = A0
      val conversion = conv
    }
}

