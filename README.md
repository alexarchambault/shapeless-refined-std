# shapeless-refined-std

*Type-level API for standard collections*

[![Build Status](https://travis-ci.org/alexarchambault/shapeless-refined-std.svg)](https://travis-ci.org/alexarchambault/shapeless-refined-std)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/alexarchambault/shapeless-refined-std?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**shapeless-refined-std** enriches the standard library, collections mostly, using the typelevel collections:
tuples, shapeless' `HList` and `Sized`.

Whenever possible, it provides alternatives with stronger types to standard library collections' methods that can either 
return collections of sizes known at compile-time, for example:

```scala
def it = Iterator.fill(nextInt(50))(nextDouble()) // A random iterator...

def result = it.slidingT(3) // Iterator[(Double, Double, Double)]
//  3 provided as argument at compile-time -> Tuple3 in the return type
    

val str = "first;second;third;fourth"

val resultOpt = str.splitH(3, ";") // Option[String :: String :: String :: HNil]
// 3 provided as argument at compile-time -> 3-length HList in the return type
```
     
or that can be repeated, for example:

```scala
val l = List.fill(nextInt(10))(nextDouble()) // A random list...
    
/* Trying to find 5 elements satisfying a predicate */
val result = l.findS(5)(_ > 0.5) // Option[Sized[List[Double], _5]]
// 5 provided as argument at compile-time -> Sized of length 5 in the return type
```
   
In the above, `splitH(3, ...)` tries to split `str` into 3 elements. `splitH` ends in `H` so it will
try to return a `HList`. If it succeeds, it returns
`Some(firstFoundElement :: secondFoundElement :: thirdFoundElement :: HNil)`, and it returns `None` if it fails. 
`findT` and `findS` also exist, where the 3-length `HList` is replaced by a tuple (`(Double, Double, Double)`) 
or a `Sized` (`Sized[List[Double], _3]`).

The returned values can be matched straight away, like in:

```scala
it.groupedT(2).map{ case (previous, current) => ... }
  
str.splitT(3, ";") match {
  case None => // failed
  case Some((first, second, third)) => // success
}
```
   
Whereas using the standard library methods, one would have had to write
  
```scala  
it.grouped(2).withPartial(false).map{ t => (t(0), t(1)) }.map{case (first, second) => ...}
   
Some(str.split(";", 3)).filter(_.length == 3).map(t => (t(0), t(1), t(2))) match {
  case None => ...
  case Some((first, second, third)) => ...
}
```

Available methods are illustrated in the tests, under `src/test`.

## Usage

Add to your `build.sbt`

```scala
libraryDependencies +=
  "com.github.alexarchambault" %% "shapeless-refined-std" % "0.1.1"
```

For the development version, add instead
```scala
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies +=
  "com.github.alexarchambault" %% "shapeless-refined-std" % "0.1.2-SNAPSHOT"
```

Then add in your sources:

```scala
import shapeless.refinedstd.syntax._
    
/* New methods ending in ...T (tuples), ...H (HList), or ...S (Sized),
 * are available on instances of TraversableOnce, Iterator, TraversableLike,
 * IterableLike, SeqLike, String, Regex. 
 */
```

Only for scala 2.11 for now, a (possibly lighter) version for scala 2.10 is being prepared. Depends on shapeless 2.1.0-RC1.


## Internals

These methods are backed by dependent functions (shapeless' `DepFn`*), available in the `shapeless.refinedstd.ops`
namespace, à la shapeless.

## Compiling

Compiling requires [my customized version](https://github.com/alexarchambault/sbt-boilerplate) of [sbt-boilerplate](https://github.com/sbt/sbt-boilerplate).
Clone it and publish it locally prior to compiling shapeless-refined-std.

---

Thanks to [@milessabin](https://github.com/milessabin/) for the suggestion of the name "shapeless-refined-std".

Copyright (c) 2014-2015 Alexandre Archambault. See LICENSE file for more details.

Released under Apache 2.0 license.

