package cmudict

class CmuDict {

  import CmuDict._

  private val wordsWithPhones: Vector[(String, String)] = readDict()

  private val phonesWithConsonants: Vector[(String, String)] =
    wordsWithPhones.map { case (word, phones) =>
      val cons = phones.split(" ").filter(arpabetConsonants contains _).mkString(" ")
      (phones, cons)
    }.distinct

  private val phonesWithVowels: Vector[(String, String)] =
    wordsWithPhones.map { case (word, phones) =>
      val vowels = phones.split(" ").filter(arpabetVowels contains _).mkString(" ")
      (phones, vowels)
    }.distinct

  private val arpaLUT: Map[String, String] = readArpaLUT()


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

  def vowelsForPhones(phones: String): String = {
    phones.split(" ")
      .filter(arpabetVowels contains _)
      .mkString(" ")
  }

  def consonantsForPhones(phones: String): String = {
    phones.split(" ")
      .filter(arpabetConsonants contains _)
      .mkString(" ")
  }

  def vowelsForWord(w: String): Vector[String] =
    phonesForWord(w).map(vowelsForPhones).distinct

  def consonantsForWord(w: String): Vector[String] =
    phonesForWord(w).map(consonantsForPhones).distinct

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


  def phonesByStrictAssonance(phones: String): Vector[String] = {
    val v = vowelsForPhones(phones)
    val matchingPhones = for {
      (phones, v2) <- phonesWithVowels
      if v == v2
    } yield phones

    matchingPhones.distinct
  }

  /**
   * Strict Assonance: words with all matching vowels (number, order, and identity)
   * Find words that match a given word w by assonance
   * @param w the word used for the assonance query
   * @return a Vector of terms with valid assonance for w
   */
  def wordsByStrictAssonance(w: String): Vector[String] = {
    // Retrieve phones exhibiting assonance
    val phones =
      phonesForWord(w)
        .flatMap(phonesByStrictAssonance)
        .distinct

    val matchingWords = for {
      p <- phones
      (w, p2) <- wordsWithPhones
      if p == p2} yield w

    matchingWords.distinct
  }

  def phonesByStrictConsonance(phones: String): Vector[String] = {
    val c = consonantsForPhones(phones)
    val matchingPhones = for {
      (phones, c2) <- phonesWithConsonants
      if c == c2
    } yield phones

    matchingPhones.distinct
  }

  /**
   * Strict Consonance: words with all matching consonants (number, order, and identity)
   * Find words that match a given word w by consonance
   * @param w the word used for the consonance query
   * @return a Vector of terms with valid consonance for w
   */
  def wordsByStrictConsonance(w: String): Vector[String] = {
    // Retrieve phones exhibiting consonance
    val phones =
      phonesForWord(w)
      .flatMap(phonesByStrictConsonance)
      .distinct

    val matchingWords = for {
      p <- phones
      (w, p2) <- wordsWithPhones
      if p == p2} yield w

    matchingWords.distinct
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

  val arpabetConsonants = Set(
    "P", "B", "T", "D", "K",
    "G", "CH", "JH", "F", "V",
    "TH", "DH", "S", "Z", "SH",
    "ZH", "HH", "M", "EM", "N",
    "EN", "NG", "ENG", "L", "EL",
    "R", "DX", "NX", "Y", "W", "Q")

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
