package cmudict

class CmuDict {

  import CmuDict._

  val wordsWithPhones: Vector[(String, String)] = readDict()

  val arpaLUT: Map[String, String] = readArpaLUT()

  def contains(w: String): Boolean = {
    val lcw = w.toLowerCase
    wordsWithPhones.find(_._1 == lcw).isDefined
  }

  def phonesForWord(w: String): Vector[String] = {
    val lcw = w.toLowerCase
    for {
      (word, phones) <- wordsWithPhones
      if word == lcw
    } yield phones
  }

  def ipaForPhones(phones: String): String =
    phones.split(" ").map(arpaLUT).mkString

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
    } yield ipaForPhones(phones)
  }

  def stressForWord(w: String): Vector[String] =
    for (phones <- phonesForWord(w)) yield stress(phones)

  def syllableCountForWord(w: String): Vector[Int] = {
    val counts = for (s <- stressForWord(w)) yield s.length
    counts.distinct
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

  /**
   * Find words that match a given word w by alliteration
   * @param w the word used for the alliteration query
   * @return a Vector of valid alliterations for w
   */
  def wordsByAlliteration(w: String): Vector[String] = {
    val alliterations = for {
      pro <- phonesForWord(w)
      onset = pro.split(" ").head
      // Must be a true a onset (i.e. NOT a vowel)
      if !arpabetVowels.contains(onset)
      (word, phones) <- wordsWithPhones
      if phones.startsWith(onset)
    } yield word
    alliterations.distinct
  }

  /**
   * Find words that match a given word w by assonance
   * @param w the word used for the assonance query
   * @return a Vector of terms with valid assonance for w
   */
  def wordsByAssonance(w: String): Vector[String] = {
    val firstVowels = for {
      pro <- phonesForWord(w)
      firstVowel <- pro.split(" ").find(arpabetVowels contains _)
    } yield firstVowel
    val assonance = for {
      fv <- firstVowels
      (word, phones) <- wordsWithPhones
      w2fv <- phones.split(" ").find(arpabetVowels contains _)
      if fv == w2fv
    } yield word
    assonance.distinct
  }
}

object CmuDict {

  def readDict(): Vector[(String, String)] = {
    val stream = getClass.getResourceAsStream("/cmudict-0.7b")
    val source = io.Source.fromInputStream(stream, "windows-1252")
    val wordsWithPhones = for {
      line <- source.getLines()
      if !line.startsWith(";;;")
      Array(word, phones) = line.split("  ")
    } yield (word.replaceAll("""\(\d+\)$""", "").toLowerCase, phones)
    val results = wordsWithPhones.toVector
    source.close()
    results
  }

  /**
   * Create a Map from arpabet ngraph -> ipa
   */
  def readArpaLUT(): Map[String, String] = {
    val stream = getClass.getResourceAsStream("/arpabet_to_ipa")
    val source = io.Source.fromInputStream(stream, "utf8")
    val pairs = for {
      line <- source.getLines()
      Array(ngraph, ipa) = line.split("\t")
    } yield (ngraph, ipa)
    val lut = pairs.toMap
    source.close()
    lut
  }

  val arpabetVowels = Set(
    "AO", "AO0", "AO1", "AO2",
    "AA", "AA0", "AA1", "AA2",
    "IY", "IY0", "IY1", "IY2",
    "UW", "UW0", "UW1", "UW2",
    "EH", "EH0", "EH1", "EH2",
    "IH", "IH0", "IH1", "IH2",
    "UH", "UH0", "UH1", "UH2",
    "AH", "AH0", "AH1", "AH2",
    "AX", "AX0", "AX1", "AX2",
    "AE", "AE0", "AE1", "AE2",
    "EY", "EY0", "EY1", "EY2",
    "AY", "AY0", "AY1", "AY2",
    "OW", "OW0", "OW1", "OW2",
    "AW", "AW0", "AW1", "AW2",
    "OY", "OY0", "OY1", "OY2",
    "ER", "ER0", "ER1", "ER2",
    "AXR", "AXR0", "AXR1", "AXR2")

  def stress(phones: String): String =
    phones.replaceAll("""[^012]""", "")

  def rhymingChunkForPhones(phones: String): String = {
    val chunks = phones.split(" ")
    for {
      i <- chunks.indices.reverse
      if chunks(i).endsWith("1") || chunks(i).endsWith("2")
    } return chunks.slice(i, chunks.length).mkString(" ")
    phones
  }

}
