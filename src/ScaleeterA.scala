/** Versão podre do paginador de tweets. Não fala o número total de tweets, só conta, e corta as palavras ao meio mesmo.
  * Ainda por cima assume que vai ter no máximo 99 tweets, e gasta 139 ao invés de 140 caracteres nos primeiros 9.
  */
object ScaleeterA extends App {

  val TWEET_LENGTH = 140
  val HEADER_LENGTH = 3
  val CHUNK_LENGTH = TWEET_LENGTH - HEADER_LENGTH

  def generateTweets(intput: TraversableOnce[String]) =
    input.mkString(" ").sliding(CHUNK_LENGTH, CHUNK_LENGTH).zipWithIndex map {
      case (ss, i) => s"${i + 1}/$ss"
    }

  val input = io.Source.stdin.getLines()

  generateTweets(input) foreach println
}

//ScaleeterA.main(args)
