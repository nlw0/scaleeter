/** Versão sofisticada, com um pouco de OO. As classes `Tweet`e `Word` permitem fazer contas mais detalhadas do
  * tamanho do tweet, e em especial permitem aqui considerar que URLs são encurtadas, consumindo apenas 23 caracteres.
  */
object ScaleeterD extends App {

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

  val TWEET_LENGTH = 140

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

  def dfsTweetPaginate(words: List[Word], page: Int, total: Int): (Int, List[Tweet]) = {
    if (page == total) {
      val finalTweet = words.foldLeft(Tweet(page, total)) { (a, b) => a :+ b }
      (finalTweet.length, List(finalTweet))
    } else {
      (1 to words.length).iterator map {
        n =>
          val (hereWords, thereWords) = words.splitAt(n)
          val thisTweet = hereWords.foldLeft(Tweet(page, total)) { (a, b) => a :+ b }
          (thisTweet, thereWords)
      } takeWhile {
        case (tw, ww) => tw.length <= TWEET_LENGTH
      } map {
        case (tw: Tweet, ww) =>
          val below = dfsTweetPaginate(ww, page + 1, total)
          (below._1 min tw.length, tw :: below._2)
      } reduce {
        (minTtA, minTtB) =>
          if (minTtA._1 >= minTtB._1) minTtA else minTtB
      }
    }
  }

  val input = io.Source.stdin.getLines()

  val wordStream = input.flatMap(_.split("\\s+")).toList map { ww => Word(ww) }

  val tweetsGreedy = generateTweets(wordStream)

  val nTweets = tweetsGreedy.size

  val tweetsOptim = dfsTweetPaginate(wordStream, 1, nTweets)
  val tweets = tweetsOptim._2.reverse

  // tweets foreach { ss => println(ss.length + "\t" + ss) }
  tweets foreach println
  //  println(tweetsOptim)
}

ScaleeterD.main(args)
