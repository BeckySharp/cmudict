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

  /**
   * Convert the arpabet-based pronunciation to IPA
   * @param w a word
   * @return a Vector of valid IPA pronunciations for w
   */
  def ipaForWord(w: String): Vector[String] = {
  val lcw = w.toLowerCase
  for {
    (word, phones) <- wordsWithPhones
    if word == lcw
    ipa = phones
      .split(" ")
      .map(arpabetToIPA)
      .mkString
  } yield ipa
  }

  def stressForWord(w: String): Vector[String] =
    for (phones <- phonesForWord(w)) yield stress(phones)

  def syllableCountForWord(w: String): Vector[Int] = {
    val syllableCounts =
      for (stress <- stressForWord(w)) yield stress.length
    syllableCounts.distinct
  }

  /**
   * A word's rhyming chunk spans the last stressed syllable to the end of the word
   */
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

  /**
   * Create a Map from arpabet ngraph -> ipa
   */
  val arpaLUT: Map[String, String] = {
    val stream = getClass.getResourceAsStream("/arpabet_to_ipa")
    val source = io.Source.fromInputStream(stream, "utf8")
    val pairs = for {
      line <- source.getLines()
      pair = line.split("\t")
    } yield (pair.head, pair.last)
    pairs.toMap
  }

  def stress(phones: String): String =
    phones.replaceAll("""[^012]""", "")

  def arpabetToIPA(arpa: String): String = arpaLUT(arpa)

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
