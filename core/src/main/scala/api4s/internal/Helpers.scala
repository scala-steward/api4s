package api4s.internal

import api4s.Media
import cats.data.Validated._
import cats.data.{ Validated, ValidatedNec }
import cats.effect.Sync
import cats.{ Applicative, FlatMap }
import fs2.Chunk
import io.chrisdavenport.vault.Vault
import io.circe.{ Decoder, Encoder, Printer }
import org.http4s._
import org.http4s.circe._
import org.http4s.headers._

object Helpers {
  def pathEncode(s: String): String = PathCodecs.encode(s)

  def pathDecode(s: String): String = PathCodecs.decode(s)

  implicit class RichRequest[F[_]](val r: Request[F]) extends AnyVal {
    def pathSegments: List[String] = r.uri.path.split('/').filter(_.nonEmpty).toList

    def decodeValidatedOpt[A](
      f: ValidatedNec[Throwable, Option[A]] => F[Response[F]]
    )(implicit F: FlatMap[F], D: EntityDecoder[F, A]): F[Response[F]] =
      r.headers.get(`Content-Length`) match {
        case Some(l) if l.length == 0 => f(Valid(None))
        case _ => r.headers.get(`Content-Type`) match {
          case None => f(Valid(None))
          case Some(_) => decodeValidated[A](x => f(x.map(Some(_))))
        }
      }

    def decodeValidated[A](
      f: ValidatedNec[Throwable, A] => F[Response[F]]
    )(implicit F: FlatMap[F], D: EntityDecoder[F, A]): F[Response[F]] =
      F.flatMap(D.decode(r, strict = true).value) {
        case Left(e) => f(Validated.invalidNec(e))
        case Right(x) => f(Valid(x))
      }
  }

  private val printer = Printer.spaces2.copy(dropNullValues = true)

  def circeEntityEncoder[F[_] : Applicative, A: Encoder]: EntityEncoder[F, A] =
    jsonEncoderWithPrinterOf[F, A](printer)

  def circeEntityDecoder[F[_] : Sync, A: Decoder]: EntityDecoder[F, A] = jsonOf[F, A]

  def jsonResponse[F[_] : Applicative, A: Encoder](status: Status, attrs: Vault)(
    a: A
  ): Response[F] = {
    val encoder = circeEntityEncoder[F, A]
    val entity = encoder.toEntity(a)

    Response(
      status = status,
      headers = encoder.headers,
      body = entity.body,
      attributes = attrs
    )
  }

  def textResponse[F[_]](status: Status, mediaType: String, attrs: Vault)(
    text: String
  ): Response[F] = {
    val encoder = EntityEncoder.simple[F, String](Header("Content-Type", mediaType))(s =>
      Chunk.bytes(s.getBytes(DefaultCharset.nioCharset))
    )

    Response(
      status = status,
      headers = encoder.headers,
      body = encoder.toEntity(text).body,
      attributes = attrs
    )
  }

  def mediaResponse[F[_]](status: Status, attrs: Vault)(media: Media[F]): Response[F] = Response(
    status = status,
    headers = media.headers,
    body = media.body,
    attributes = attrs
  )

  def emptyResponse[F[_]](status: Status, attrs: Vault): Response[F] =
    Response(status = status, attributes = attrs)
}
