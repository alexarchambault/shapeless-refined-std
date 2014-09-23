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

import scala.language.higherKinds
import scala.collection.IterableLike

/**
 * Evidence that `Repr` can be viewed as an `IterableLike[A, Repr]` for some type `A`.
 */
trait IsIterableLike[Repr] {
  type A
  val conversion: Repr => IterableLike[A, Repr]
}

object IsIterableLike {
  type Aux[Repr, A0] = IsIterableLike[Repr] { type A = A0 }

  def apply[Repr] (implicit 
    isIterableLike: IsIterableLike[Repr]
  ): Aux[Repr, isIterableLike.A] = isIterableLike

  implicit val stringRepr: Aux[String, Char] =
    new IsIterableLike[String] {
      type A = Char
      val conversion = implicitly[String => IterableLike[Char, String]]
    }

  implicit def genTraversableLikeRepr[C[_], A0] (implicit 
    conv: C[A0] => IterableLike[A0, C[A0]]
  ): Aux[C[A0], A0] =
    new IsIterableLike[C[A0]] {
      type A = A0
      val conversion = conv
    }
}

