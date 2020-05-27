package io.lemonlabs.uri.typesafe

import cats.Contravariant
import cats.syntax.contravariant._
import shapeless._
import shapeless.labelled._
import shapeless.ops.coproduct.Reify
import shapeless.ops.hlist.ToList

import scala.language.implicitConversions

@ _root_.scala.annotation.implicitNotFound("Could not find an instance of QueryKey for ${A}")
trait QueryKey[A] extends _root_.scala.Any with _root_.scala.Serializable {
  def queryKey(a: A): String
};
object QueryKey extends QueryKeyInstances {
  @scala.inline
  def apply[A](implicit instance: QueryKey[A]): QueryKey[A] = instance;

  trait BaseOps[A] {
    type TypeClassType <: QueryKey[A];
    val typeClassInstance: TypeClassType;

    def self: A;

    def queryKey: String = typeClassInstance.queryKey(self)
  };

  trait ToQueryKeyOps {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toQueryKeyOps[A](target: A)(implicit tc: QueryKey[A]): BaseOps[A] {
      type TypeClassType = QueryKey[A]
    } = {
      final class $anon extends BaseOps[A] {
        type TypeClassType = QueryKey[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  };

  object nonInheritedOps extends ToQueryKeyOps;

  trait AllOps[A] extends BaseOps[A] {
    type TypeClassType <: QueryKey[A];
    val typeClassInstance: TypeClassType
  };

  object ops {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toAllQueryKeyOps[A](target: A)(implicit tc: QueryKey[A]): AllOps[A] {
      type TypeClassType = QueryKey[A]
    } = {
      final class $anon extends AllOps[A] {
        type TypeClassType = QueryKey[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  }

}

sealed trait QueryKeyInstances1 {
  implicit val contravariant: Contravariant[QueryKey] = new Contravariant[QueryKey] {
    def contramap[A, B](fa: QueryKey[A])(f: B => A): QueryKey[B] = (b: B) => fa.queryKey(f(b))
  }
}

sealed trait QueryKeyInstances extends QueryKeyInstances1 {
  implicit val stringQueryKey: QueryKey[String] = a => a
  implicit final val booleanQueryValue: QueryKey[Boolean] = stringQueryKey.contramap(_.toString)
  implicit final val charQueryValue: QueryKey[Char] = stringQueryKey.contramap(_.toString)
  implicit final val intQueryValue: QueryKey[Int] = stringQueryKey.contramap(_.toString)
  implicit final val longQueryValue: QueryKey[Long] = stringQueryKey.contramap(_.toString)
  implicit final val floatQueryValue: QueryKey[Float] = stringQueryKey.contramap(_.toString)
  implicit final val doubleQueryValue: QueryKey[Double] = stringQueryKey.contramap(_.toString)
  implicit final val uuidQueryValue: QueryKey[java.util.UUID] = stringQueryKey.contramap(_.toString)
}

@ _root_.scala.annotation.implicitNotFound("Could not find an instance of QueryValue for ${A}")
trait QueryValue[-A] extends _root_.scala.Any with _root_.scala.Serializable { self =>
  def queryValue(a: A): Option[String]
}
object QueryValue extends QueryValueInstances {
  def derive[A]: Derivation[A] = new Derivation[A](());
  class Derivation[A](private val dummy: Unit) extends AnyVal {
    def by[C <: Coproduct, R <: HList](
        key: _root_.scala.Function1[A, String]
    )(implicit gen: Generic.Aux[A, C], reify: Reify.Aux[C, R], toList: ToList[R, A]): QueryValue[A] =
      a => toList(reify()).iterator.map((x => x.->(key(x)))).toMap.get(a)
  };
  @scala.inline
  def apply[A](implicit instance: QueryValue[A]): QueryValue[A] = instance;
  trait BaseOps[A] {
    type TypeClassType <: QueryValue[A];
    val typeClassInstance: TypeClassType;
    def self: A;
    def queryValue: Option[String] = typeClassInstance.queryValue(self)
  };
  trait ToQueryValueOps {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toQueryValueOps[A](target: A)(implicit tc: QueryValue[A]): BaseOps[A] {
      type TypeClassType = QueryValue[A]
    } = {
      final class $anon extends BaseOps[A] {
        type TypeClassType = QueryValue[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  };
  object nonInheritedOps extends ToQueryValueOps;
  trait AllOps[A] extends BaseOps[A] {
    type TypeClassType <: QueryValue[A];
    val typeClassInstance: TypeClassType
  };
  object ops {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toAllQueryValueOps[A](target: A)(implicit tc: QueryValue[A]): AllOps[A] {
      type TypeClassType = QueryValue[A]
    } = {
      final class $anon extends AllOps[A] {
        type TypeClassType = QueryValue[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  }
}

sealed trait QueryValueInstances2 {
  implicit val contravariant: Contravariant[QueryValue] = new Contravariant[QueryValue] {
    def contramap[A, B](fa: QueryValue[A])(f: B => A): QueryValue[B] = (b: B) => fa.queryValue(f(b))
  }
}

sealed trait QueryValueInstances1 extends QueryValueInstances2 {
  implicit final val stringQueryValue: QueryValue[String] = Option(_)
  implicit final val booleanQueryValue: QueryValue[Boolean] = stringQueryValue.contramap(_.toString)
  implicit final val charQueryValue: QueryValue[Char] = stringQueryValue.contramap(_.toString)
  implicit final val intQueryValue: QueryValue[Int] = stringQueryValue.contramap(_.toString)
  implicit final val longQueryValue: QueryValue[Long] = stringQueryValue.contramap(_.toString)
  implicit final val floatQueryValue: QueryValue[Float] = stringQueryValue.contramap(_.toString)
  implicit final val doubleQueryValue: QueryValue[Double] = stringQueryValue.contramap(_.toString)
  implicit final val uuidQueryValue: QueryValue[java.util.UUID] = stringQueryValue.contramap(_.toString)
  implicit final val noneQueryValue: QueryValue[None.type] = _ => None
}

sealed trait QueryValueInstances extends QueryValueInstances1 {
  implicit final def optionQueryValue[A: QueryValue]: QueryValue[Option[A]] = _.flatMap(QueryValue[A].queryValue)
}

@ _root_.scala.annotation.implicitNotFound("Could not find an instance of QueryKeyValue for ${A}")
trait QueryKeyValue[A] extends _root_.scala.Any with _root_.scala.Serializable {
  def queryKey(a: A): String;
  def queryValue(a: A): Option[String];
  def queryKeyValue(a: A): scala.Tuple2[String, Option[String]] = queryKey(a).->(queryValue(a))
};
object QueryKeyValue extends QueryKeyValueInstances {
  def apply[T, K, V](toKey: _root_.scala.Function1[T, K], toValue: _root_.scala.Function1[T, V])(implicit
      evidence$2: QueryKey[K],
      evidence$3: QueryValue[V]
  ): QueryKeyValue[T] = {
    final class $anon extends QueryKeyValue[T] {
      def queryKey(a: T): String = QueryKey[K].queryKey(toKey(a));
      def queryValue(a: T): Option[String] = QueryValue[V].queryValue(toValue(a))
    };
    new $anon()
  }
  @scala.inline
  def apply[A](implicit instance: QueryKeyValue[A]): QueryKeyValue[A] = instance;
  trait BaseOps[A] {
    type TypeClassType <: QueryKeyValue[A];
    val typeClassInstance: TypeClassType;
    def self: A;
    def queryKey: String = typeClassInstance.queryKey(self);
    def queryValue: Option[String] = typeClassInstance.queryValue(self);
    def queryKeyValue: scala.Tuple2[String, Option[String]] = typeClassInstance.queryKeyValue(self)
  };
  trait ToQueryKeyValueOps {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toQueryKeyValueOps[A](target: A)(implicit tc: QueryKeyValue[A]): BaseOps[A] {
      type TypeClassType = QueryKeyValue[A]
    } = {
      final class $anon extends BaseOps[A] {
        type TypeClassType = QueryKeyValue[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  };
  object nonInheritedOps extends ToQueryKeyValueOps;
  trait AllOps[A] extends BaseOps[A] {
    type TypeClassType <: QueryKeyValue[A];
    val typeClassInstance: TypeClassType
  };
  object ops {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toAllQueryKeyValueOps[A](target: A)(implicit tc: QueryKeyValue[A]): AllOps[A] {
      type TypeClassType = QueryKeyValue[A]
    } = {
      final class $anon extends AllOps[A] {
        type TypeClassType = QueryKeyValue[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  }
}

sealed trait QueryKeyValueInstances {
  implicit def tuple2QueryKeyValue[K: QueryKey, V: QueryValue]: QueryKeyValue[(K, V)] =
    QueryKeyValue(_._1, _._2)
}

@ _root_.scala.annotation.implicitNotFound("Could not find an instance of TraversableParams for ${A}")
trait TraversableParams[A] extends _root_.scala.Any with _root_.scala.Serializable {
  def toSeq(a: A): Seq[scala.Tuple2[String, Option[String]]];
  def toVector(a: A): Vector[scala.Tuple2[String, Option[String]]] = toSeq(a).toVector
};
object TraversableParams extends TraversableParamsInstances {
  implicit def field[K <: Symbol, V](implicit K: Witness.Aux[K], V: QueryValue[V]): TraversableParams[FieldType[K, V]] =
    (a: FieldType[K, V]) => List(K.value.name.->(V.queryValue(a)));
  implicit def sub[K <: Symbol, V](implicit
      K: Witness.Aux[K],
      V: TraversableParams[V]
  ): TraversableParams[FieldType[K, V]] = (a: FieldType[K, V]) => V.toSeq(a);
  implicit val hnil: TraversableParams[HNil] = (x$20: HNil) => List.empty;
  implicit def hcons[H, T <: HList](implicit
      H: TraversableParams[H],
      T: TraversableParams[T]
  ): TraversableParams[::[H, T]] = (a: ::[H, T]) => H.toSeq(a.head).++(T.toSeq(a.tail));
  def product[A, R <: HList](implicit gen: LabelledGeneric.Aux[A, R], R: TraversableParams[R]): TraversableParams[A] =
    (a: A) => R.toSeq(gen.to(a));
  @scala.inline
  def apply[A](implicit instance: TraversableParams[A]): TraversableParams[A] = instance;
  trait BaseOps[A] {
    type TypeClassType <: TraversableParams[A];
    val typeClassInstance: TypeClassType;
    def self: A;
    def toSeq: Seq[scala.Tuple2[String, Option[String]]] = typeClassInstance.toSeq(self);
    def toVector: Vector[scala.Tuple2[String, Option[String]]] = typeClassInstance.toVector(self)
  };
  trait ToTraversableParamsOps {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toTraversableParamsOps[A](target: A)(implicit tc: TraversableParams[A]): BaseOps[A] {
      type TypeClassType = TraversableParams[A]
    } = {
      final class $anon extends BaseOps[A] {
        type TypeClassType = TraversableParams[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  };
  object nonInheritedOps extends ToTraversableParamsOps;
  trait AllOps[A] extends BaseOps[A] {
    type TypeClassType <: TraversableParams[A];
    val typeClassInstance: TypeClassType
  };
  object ops {
    @java.lang.SuppressWarnings(
      scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")
    )
    implicit def toAllTraversableParamsOps[A](target: A)(implicit tc: TraversableParams[A]): AllOps[A] {
      type TypeClassType = TraversableParams[A]
    } = {
      final class $anon extends AllOps[A] {
        type TypeClassType = TraversableParams[A];
        val self = target;
        val typeClassInstance: TypeClassType = tc
      };
      new $anon()
    }
  }
}

sealed trait TraversableParamsInstances1 {
  implicit val contravariant: Contravariant[TraversableParams] = new Contravariant[TraversableParams] {
    override def contramap[A, B](fa: TraversableParams[A])(f: B => A): TraversableParams[B] = b => fa.toSeq(f(b))
  }
}

sealed trait TraversableParamsInstances extends TraversableParamsInstances1 {
  implicit def iterableTraversableParams[A](implicit tc: QueryKeyValue[A]): TraversableParams[Iterable[A]] =
    (ax: Iterable[A]) => ax.map((a: A) => tc.queryKey(a) -> tc.queryValue(a)).toSeq

  implicit def seqTraversableParams[A](implicit tc: QueryKeyValue[A]): TraversableParams[Seq[A]] =
    (ax: Seq[A]) => ax.map((a: A) => tc.queryKey(a) -> tc.queryValue(a))

  implicit def listTraversableParams[A](implicit tc: QueryKeyValue[A]): TraversableParams[List[A]] =
    (ax: List[A]) => ax.map((a: A) => tc.queryKey(a) -> tc.queryValue(a))

  implicit def singleTraversableParams[A](implicit tc: QueryKeyValue[A]): TraversableParams[A] =
    (a: A) => Seq(tc.queryKey(a) -> tc.queryValue(a))

  implicit def vectorTraversableParams[A](implicit tc: QueryKeyValue[A]): TraversableParams[Vector[A]] =
    (ax: Vector[A]) => ax.map((a: A) => tc.queryKey(a) -> tc.queryValue(a))

  implicit def mapTraversableParams[K, V](implicit tck: QueryKey[K], tcv: QueryValue[V]): TraversableParams[Map[K, V]] =
    (ax: Map[K, V]) => ax.map { case (k, v) => tck.queryKey(k) -> tcv.queryValue(v) }.toSeq
}
