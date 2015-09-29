/** Versão caprichada do paginador de tweets. Total impresso em todos os tweets, e cortes feitos nos espaços em
  * branco. Baseado em um processo recursivo que vai re-paginando os tweets com a nova informação do número de
  * páginas, até convergir. Assim não precisa nem assumir menos de 100 tweets.
  */
object ScaleeterB extends App {

  val TWEET_LENGTH = 140

  def generateHeader(n: Int, total: Option[Int] = None) = s"$n/${total.getOrElse("x")}"

  @annotation.tailrec
  def generateTweets(input: Traversable[String], total: Option[Int] = None): List[String] = {
    val tweets = generateTweetsSinglePass(wordStream, total)
    val currentSize = tweets.size
    if (total contains currentSize) tweets.reverse
    else generateTweets(input, Some(currentSize))
  }

  def generateTweetsSinglePass(input: TraversableOnce[String], total: Option[Int] = None): List[String] = {
    wordStream.foldLeft(List[String]()) {
      case (Nil, word) =>
        val page = 1
        generateHeader(page, total) + " " + word :: Nil
      case (tt :: ts, word) =>
        val page = ts.size + 2
        val spaceLeft = TWEET_LENGTH - tt.length
        if (word.length + 1 < spaceLeft)
          tt + " " + word :: ts
        else
          generateHeader(page, total) + " " + word :: tt :: ts
    }
  }

  val input = io.Source.stdin.getLines()

  val wordStream = input.flatMap(_.split("\\s+")).toVector

  val tweets = generateTweets(wordStream)

  tweets foreach println
}

ScaleeterB.main(args)
