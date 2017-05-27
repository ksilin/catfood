package com.example.catfood.existentials

import java.time.Instant
import java.util.Date

import org.scalatest.{ FreeSpec, MustMatchers }

class ExistentialTypes extends FreeSpec with MustMatchers {

//  http://www.cakesolutions.net/teamblogs/existential-types-in-scala
  // https://gist.github.com/pjrt/269ddd1d8036374c648dbf6d52fb388f

  // this interface need some wrapping and restricting
  object JavaInterface {
    def bind(o: Object*): Unit =
      println(s"bound $o")
  }

  "raw java interface can be called with whatever" in {

    JavaInterface.bind("x")
    JavaInterface.bind(Boolean.box(true))
    case class Misfit(x: Int)
    JavaInterface.bind(Misfit(123))
    JavaInterface.bind(new Object { val z = 1 })
  }

  "we want to restrict access to it by wrapping and defining allowed types" in {
//    object WrappedInterface {
//      def safeBind(a: AllowedType*) = ???
//    }

//    define a trait for the AllowedType

    sealed trait AllowedType[A] {

      // must be convertible to null
      // must not be a primitive
      type JavaType >: Null <: AnyRef

      def toJavaType(a: A): JavaType

      // upcast
      def toObject(a: A): Object = toJavaType(a)
    }
    object AllowedType {

      def apply[A](implicit evidence: AllowedType[A]): AllowedType[A] = evidence

      def instance[A, J >: Null <: AnyRef](f: A => J): AllowedType[A] =
        new AllowedType[A] {
          override type JavaType = J
          override def toJavaType(a: A): JavaType = f(a)
        }

      implicit val intInstance: AllowedType[Int]         = instance(Int.box)
      implicit val strInstance: AllowedType[String]      = instance(identity)
      implicit val boolInstance: AllowedType[Boolean]    = instance(Boolean.box)
      implicit val instantInstance: AllowedType[Instant] = instance(Date.from)
      // this is why we need the :> Null restriction
      implicit def optionInstance[A](implicit evidence: AllowedType[A]): AllowedType[Option[A]] =
        instance[Option[A], evidence.JavaType](s => s.map(r => evidence.toJavaType(r)).orNull)
    }

    trait AnyAllowedType {
      type A
      val value: A
      val evidence: AllowedType[A]
    }

    final case class MkAnyAllowedType[A0](value: A0)(implicit val evidence: AllowedType[A0])
        extends AnyAllowedType { type A = A0 }

    // since now we require an evidence, we cannot pass just any type:
    val ints: AnyAllowedType = MkAnyAllowedType(1)
    println(ints)
    val strs: AnyAllowedType = MkAnyAllowedType("str")
    println(strs)
    val instant: AnyAllowedType = MkAnyAllowedType(Instant.now())
    println(instant)
    case class Misfit(x: Int)
    // Error: could not find implicit value for parameter evidence: AllowedType[Misfit]
    """MkAnyAllowedType(Misfit(1))""" mustNot compile

    // implicit conversion
    object AnyAllowedType {

      implicit val typeToObject: (AnyAllowedType) => Object = { t: AnyAllowedType =>
        t.evidence.toObject(t.value)
      }

      implicit val anyAllowedInstance: AllowedType[AnyAllowedType] =
        AllowedType.instance(typeToObject)

//      implicit def anyAllowedToAny[A: AnyAllowedType](a: A): AnyAllowedType =
//        AnyAllowedType(a) //.instance(ex => ex.evidence(a.value))
    }


    object SafeInterface {
      import AnyAllowedType._

      def safeBind(v: AnyAllowedType*): Unit = {
        //        val typeToObject = { t: AnyAllowedType =>
        //          t.evidence.toObject(t.value)
        //        }
        val jo = v map typeToObject
        JavaInterface.bind(jo)
      }
    }
    SafeInterface.safeBind(ints)
    SafeInterface.safeBind(strs)
    """SafeInterface.safeBind("hi")""" mustNot compile

  }

}
