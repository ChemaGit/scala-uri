package io.lemonlabs.uri.typesafe

import cats.Contravariant

import scala.language.implicitConversions

@ _root_.scala.annotation.implicitNotFound("Could not find an instance of Fragment for ${A}")
trait Fragment[A] extends _root_.scala.Any with _root_.scala.Serializable { self =>
  def fragment(a: A): Option[String];
  def contramap[B](f: _root_.scala.Function1[B, A]): Fragment[B] = (b: B) => self.fragment(f(b))
}

object Fragment extends FragmentInstances {
  @scala.inline
  def apply[A](implicit instance: Fragment[A]): Fragment[A] = instance;
  trait BaseOps[A] {
    type TypeClassType <: Fragment[A];
    val typeClassInstance: TypeClassType;
    def self: A;
    def fragment: Option[String] = typeClassInstance.fragment(self)
  };
  trait ToFragmentOps {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toFragmentOps[A](target: A)(implicit tc: Fragment[A]): BaseOps[A] {
      type TypeClassType = Fragment[A]
    } = {
      final class $anon extends BaseOps[A] {
        type TypeClassType = Fragment[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  };
  object nonInheritedOps extends ToFragmentOps;
  trait AllOps[A] extends BaseOps[A] {
    type TypeClassType <: Fragment[A];
    val typeClassInstance: TypeClassType
  };
  object ops {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toAllFragmentOps[A](target: A)(implicit tc: Fragment[A]): AllOps[A] {
      type TypeClassType = Fragment[A]
    } = {
      final class $anon extends AllOps[A] {
        type TypeClassType = Fragment[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  }
}

sealed trait FragmentInstances2 {
  implicit val contravariant: Contravariant[Fragment] = new Contravariant[Fragment] {
    def contramap[A, B](fa: Fragment[A])(f: B => A): Fragment[B] = b => fa.fragment(f(b))
  }
}

sealed trait FragmentInstances1 extends FragmentInstances2 {
  implicit val stringFragment: Fragment[String] = a => Option(a)
  implicit final val booleanQueryValue: Fragment[Boolean] = stringFragment.contramap(_.toString)
  implicit final val charQueryValue: Fragment[Char] = stringFragment.contramap(_.toString)
  implicit final val intQueryValue: Fragment[Int] = stringFragment.contramap(_.toString)
  implicit final val longQueryValue: Fragment[Long] = stringFragment.contramap(_.toString)
  implicit final val floatQueryValue: Fragment[Float] = stringFragment.contramap(_.toString)
  implicit final val doubleQueryValue: Fragment[Double] = stringFragment.contramap(_.toString)
  implicit final val uuidQueryValue: Fragment[java.util.UUID] = stringFragment.contramap(_.toString)
  implicit val noneFragment: Fragment[None.type] = identity
}

sealed trait FragmentInstances extends FragmentInstances1 {
  implicit def optionFragment[A: Fragment]: Fragment[Option[A]] = _.flatMap(Fragment[A].fragment)
}
