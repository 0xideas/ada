package ada.enhancements

import ada.interface._

import io.circe._
import io.circe.generic.semiauto._
import io.circe.{ Decoder, Encoder }


class GetSetTree[A]{

    implicit val stubDecoder: Decoder[Stub[A]] = deriveDecoder[Stub[A]]
    implicit val stubEncoder: Encoder[Stub[A]] = deriveEncoder[Stub[A]]

    implicit def leafDecoder(implicit actionDecoder: Decoder[A]): Decoder[Leaf[A]] = deriveDecoder[Leaf[A]]
    implicit def leafEncoder(implicit actionEncoder: Encoder[A]): Encoder[Leaf[A]] = deriveEncoder[Leaf[A]]

    implicit def twigDecoder(implicit actionDecoder: Decoder[A]): Decoder[Twig[A]] = deriveDecoder[Twig[A]]
    implicit def twigEncoder(implicit actionEncoder: Encoder[A]): Encoder[Twig[A]] = deriveEncoder[Twig[A]]

    implicit def branchDecoder(implicit actionDecoder: Decoder[A]): Decoder[Branch[A]] = deriveDecoder[Branch[A]]
    implicit def branchEncoder(implicit actionEncoder: Encoder[A]): Encoder[Branch[A]] = deriveEncoder[Branch[A]]

    case class namedTree[A](name: String, tree: Tree[A])
    implicit def namedTreeDecoder(implicit actionDecoder: Decoder[A]) = deriveDecoder[namedTree[A]]
    implicit def namedTreeEncoder(implicit actionEncoder: Encoder[A]) = deriveEncoder[namedTree[A]]

    implicit def treeDecoder(implicit actionDecoder: Decoder[A]): Decoder[Tree[A]] = new Decoder[Tree[A]] {
        final def apply(c: HCursor): Decoder.Result[Tree[A]] = {
            for {
                name <- c.downField("name").as[String]
                tree <- c.downField("tree").as[Json]
            } yield {
                val result = name match {
                    case "stub" => stubDecoder(tree.hcursor)
                    case "leaf" => leafDecoder(actionDecoder)(tree.hcursor)
                    case "twig" => twigDecoder(actionDecoder)(tree.hcursor)
                    case "branch" => branchDecoder(actionDecoder)(tree.hcursor)
                }
                result match {
                    case Right(treeOut) => treeOut
                    case Left(error) => throw error
                }
            }                
        } 
    }
    implicit def treeEncoder(implicit actionEncoder: Encoder[A]): Encoder[Tree[A]] = new Encoder[Tree[A]] {
            final def apply(tree: Tree[A]): Json = {
                tree match {
                    case stub: Stub[A] => Json.fromFields(Iterable(("name", Json.fromString("stub")), ("tree", stubEncoder(stub) )))
                    case leaf: Leaf[A] => Json.fromFields(Iterable(("name", Json.fromString("leaf")), ("tree", leafEncoder(actionEncoder)(leaf) )))
                    case twig: Twig[A] => Json.fromFields(Iterable(("name", Json.fromString("twig")), ("tree", twigEncoder(actionEncoder)(twig) )))
                    case branch: Branch[A] => Json.fromFields(Iterable(("name", Json.fromString("branch")), ("tree", branchEncoder(actionEncoder)(branch) )))
                }
            }
        }

}