/** Versão sofisticada, com um pouco de OO. As classes `Tweet`e `Word` permitem fazer contas mais detalhadas do
  * tamanho do tweet, e em especial permitem aqui considerar que URLs são encurtadas, consumindo apenas 23 caracteres.
  */
object ScaleeterC extends App {

  val TWEET_LENGTH = 140

  case class Word(value: String) {
    val urlRegex =
      raw"""(http|ftp|https):\/\/[\w\-_]+(\.[\w\-_]+)+([\w\-\.,@?^=%&amp;:/~\+#]*[\w\-\@?^=%&amp;/~\+#])?""".r

    lazy val length = urlRegex.replaceAllIn(value, "http://t.co/abcdefghijk").length

    override def toString = value
  }

  case class Tweet(page: Int, total: Int, content: List[Word] = Nil, contentLength: Int = 0) {
    def header = s"$page/$total"

    def `:+`(word: Word) =
      Tweet(page, total, content :+ word, contentLength + word.length + (if (content.isEmpty) 0 else 1))

    override def toString = header + " " + content.map(_.value).mkString(" ")

    lazy val length = header.length + 1 + contentLength
  }

  @annotation.tailrec
  def generateTweets(input: Traversable[Word], total: Int = 0): List[Tweet] = {
    val tweets = generateTweetsSinglePass(wordStream, total)
    val currentSize = tweets.size
    if (total == currentSize) tweets.reverse
    else generateTweets(input, currentSize)
  }

  def generateTweetsSinglePass(input: TraversableOnce[Word], total: Int = 0): List[Tweet] = {
    wordStream.foldLeft(List[Tweet]()) {
      case (Nil, word) =>
        (Tweet(1, total) :+ word) :: Nil
      case (tt :: ts, word) =>
        val spaceLeft = TWEET_LENGTH - tt.length
        if (word.length + 1 <= spaceLeft)
          (tt :+ word) :: ts
        else
          (Tweet(ts.size + 2, total) :+ word) :: tt :: ts
    }
  }

  val input = io.Source.stdin.getLines()

  val wordStream = input.flatMap(_.split("\\s+")).toVector map { ww => Word(ww) }

  val tweets = generateTweets(wordStream)

  tweets foreach println
}

ScaleeterC.main(args)
