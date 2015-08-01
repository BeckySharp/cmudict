package cmudict

class CmuDict {

  import CmuDict._

  val wordsWithPhones: Vector[(String, String)] = readDict()

  def phonesForWord(w: String): Vector[String] = {
    val lcw = w.toLowerCase
    for {
      (word, phones) <- wordsWithPhones
      if word == lcw
    } yield phones
  }

  def stressForWord(w: String): Vector[String] =
    for (phones <- phonesForWord(w)) yield stress(phones)

  def syllableCountForWord(w: String): Vector[Int] =
    for (stress <- stressForWord(w)) yield stress.length

  def rhymingChunksForWord(w: String): Vector[String] =
    for (phones <- phonesForWord(w)) yield rhymingChunkForPhones(phones)

  def wordsByStress(s: String): Vector[String] = {
    val words = for {
      (word, phones) <- wordsWithPhones
      if stress(phones) == s
    } yield word
    words.distinct
  }

  def wordsByRhymingChunk(chunk: String): Vector[String] = {
    val words = for {
      (word, phones) <- wordsWithPhones
      if phones endsWith chunk
    } yield word
    words.distinct
  }

  def wordsByRhyme(w: String): Vector[String] = {
    val words = for {
      chunk <- rhymingChunksForWord(w)
      word <- wordsByRhymingChunk(chunk)
      if word != w
    } yield word
    words.distinct
  }

}

object CmuDict {

  def readDict(): Vector[(String, String)] = {
    val stream = getClass.getResourceAsStream("/cmudict-0.7b")
    val source = io.Source.fromInputStream(stream, "windows-1252")
    val wordsWithPhones = for {
      line <- source.getLines()
      if !line.startsWith(";;;")
    } yield {
      val Array(word, phones) = line.split("  ")
      (word.replaceAll("""\(\d+\)$""", "").toLowerCase, phones)
    }
    val results = wordsWithPhones.toVector
    source.close()
    results
  }

  def stress(phones: String): String =
    phones.replaceAll("""[^012]""", "")

  def rhymingChunkForPhones(phones: String): String = {
    val chunks = phones.split(" ")
    val stress = "[12]$".r
    for {
      i <- chunks.indices.reverse
      if stress.findFirstIn(chunks(i)).nonEmpty
    } return chunks.slice(i, chunks.length).mkString(" ")
    phones
  }

}
