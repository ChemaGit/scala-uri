package io.lemonlabs.uri.typesafe

import java.util.UUID

import cats.Contravariant
import cats.syntax.contravariant._
import shapeless._
import shapeless.labelled._

import scala.language.implicitConversions

@ _root_.scala.annotation.implicitNotFound("Could not find an instance of PathPart for ${A}")
trait PathPart[-A] extends _root_.scala.Any with _root_.scala.Serializable {
  def path(a: A): String;
  def splitPath(a: A): Seq[String] = path(a).split('/').toSeq
};
object PathPart extends PathPartInstances {
  @scala.inline
  def apply[A](implicit instance: PathPart[A]): PathPart[A] = instance;
  trait BaseOps[A] {
    type TypeClassType <: PathPart[A];
    val typeClassInstance: TypeClassType;
    def self: A;
    def path: String = typeClassInstance.path(self);
    def splitPath: Seq[String] = typeClassInstance.splitPath(self)
  };
  trait ToPathPartOps {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toPathPartOps[A](target: A)(implicit tc: PathPart[A]): BaseOps[A] {
      type TypeClassType = PathPart[A]
    } = {
      final class $anon extends BaseOps[A] {
        type TypeClassType = PathPart[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  };
  object nonInheritedOps extends ToPathPartOps;
  trait AllOps[A] extends BaseOps[A] {
    type TypeClassType <: PathPart[A];
    val typeClassInstance: TypeClassType
  };
  object ops {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toAllPathPartOps[A](target: A)(implicit tc: PathPart[A]): AllOps[A] {
      type TypeClassType = PathPart[A]
    } = {
      final class $anon extends AllOps[A] {
        type TypeClassType = PathPart[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  }
}

sealed trait PathPartInstances2 {
  implicit val contravariant: Contravariant[PathPart] = new Contravariant[PathPart] {
    def contramap[A, B](fa: PathPart[A])(f: B => A): PathPart[B] = b => fa.path(f(b))
  }
}

sealed trait PathPartInstances1 extends PathPartInstances2 {
  implicit val stringPathPart: PathPart[String] = a => a
  implicit val booleanPathPart: PathPart[Boolean] = stringPathPart.contramap(_.toString)
  implicit val charPathPart: PathPart[Char] = stringPathPart.contramap(_.toString)
  implicit val intPathPart: PathPart[Int] = stringPathPart.contramap(_.toString)
  implicit val longPathPart: PathPart[Long] = stringPathPart.contramap(_.toString)
  implicit val floatPathPart: PathPart[Float] = stringPathPart.contramap(_.toString)
  implicit val doublePathPart: PathPart[Double] = stringPathPart.contramap(_.toString)
  implicit val uuidPathPart: PathPart[UUID] = stringPathPart.contramap(_.toString)
}

sealed trait PathPartInstances extends PathPartInstances1 {
  implicit def optionPathPart[A: PathPart]: PathPart[Option[A]] = a => a.map(PathPart[A].path).getOrElse("")
}

sealed trait TraversablePathPartsInstances {
  implicit def singleTraversablePathParts[A](implicit tc: PathPart[A]): TraversablePathParts[A] =
    a => tc.splitPath(a)

  implicit def iterableTraversablePathParts[A](implicit tc: PathPart[A]): TraversablePathParts[Iterable[A]] =
    (ax: Iterable[A]) => ax.flatMap(tc.splitPath).toSeq

  implicit def seqTraversablePathParts[A](implicit tc: PathPart[A]): TraversablePathParts[Seq[A]] =
    (ax: Seq[A]) => ax.flatMap(tc.splitPath)

  implicit def vectorTraversablePathParts[A](implicit tc: PathPart[A]): TraversablePathParts[Vector[A]] =
    (ax: Vector[A]) => ax.flatMap(tc.splitPath)

  implicit def listTraversablePathParts[A](implicit tc: PathPart[A]): TraversablePathParts[List[A]] =
    (ax: List[A]) => ax.flatMap(tc.splitPath)
}

@ _root_.scala.annotation.implicitNotFound("Could not find an instance of TraversablePathParts for ${A}")
trait TraversablePathParts[A] extends _root_.scala.Any with _root_.scala.Serializable {
  def toSeq(a: A): Seq[String];
  def toVector(a: A): Vector[String] = toSeq(a).toVector
};
object TraversablePathParts extends TraversablePathPartsInstances {
  implicit def field[K <: Symbol, V](implicit
      K: Witness.Aux[K],
      V: PathPart[V]
  ): TraversablePathParts[FieldType[K, V]] = (a: FieldType[K, V]) => V.splitPath(a);
  implicit def sub[K <: Symbol, V](implicit
      K: Witness.Aux[K],
      V: TraversablePathParts[V]
  ): TraversablePathParts[FieldType[K, V]] = (a: FieldType[K, V]) => V.toSeq(a);
  implicit val hnil: TraversablePathParts[HNil] = (x$8: HNil) => Seq.empty;
  implicit def hcons[H, T <: HList](implicit
      H: TraversablePathParts[H],
      T: TraversablePathParts[T]
  ): TraversablePathParts[::[H, T]] = (a: ::[H, T]) => H.toSeq(a.head).++(T.toSeq(a.tail));
  def product[A, R <: HList](implicit gen: Generic.Aux[A, R], R: TraversablePathParts[R]): TraversablePathParts[A] =
    (a: A) => R.toSeq(gen.to(a));
  @scala.inline
  def apply[A](implicit instance: TraversablePathParts[A]): TraversablePathParts[A] = instance;
  trait BaseOps[A] {
    type TypeClassType <: TraversablePathParts[A];
    val typeClassInstance: TypeClassType;
    def self: A;
    def toSeq: Seq[String] = typeClassInstance.toSeq(self);
    def toVector: Vector[String] = typeClassInstance.toVector(self)
  };
  trait ToTraversablePathPartsOps {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toTraversablePathPartsOps[A](target: A)(implicit tc: TraversablePathParts[A]): BaseOps[A] {
      type TypeClassType = TraversablePathParts[A]
    } = {
      final class $anon extends BaseOps[A] {
        type TypeClassType = TraversablePathParts[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  };
  object nonInheritedOps extends ToTraversablePathPartsOps;
  trait AllOps[A] extends BaseOps[A] {
    type TypeClassType <: TraversablePathParts[A];
    val typeClassInstance: TypeClassType
  };
  object ops {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toAllTraversablePathPartsOps[A](target: A)(implicit tc: TraversablePathParts[A]): AllOps[A] {
      type TypeClassType = TraversablePathParts[A]
    } = {
      final class $anon extends AllOps[A] {
        type TypeClassType = TraversablePathParts[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  }
}
