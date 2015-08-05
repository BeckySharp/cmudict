package cmudict

import com.sun.xml.internal.ws.util.StringUtils
import scala.collection.mutable.ArrayBuffer

class CmuDict {

  import CmuDict._

  private val wordsWithPhones: Vector[(String, String)] = readDict()

  private val wordsWithConsonants: Vector[(String, String)] =
    wordsWithPhones.map {
      case (word, phones) => (word, consonantsForPhones(phones))
    }.distinct

  private val wordsWithVowels: Vector[(String, String)] =
    wordsWithPhones.map {
      case (word, phones) => (word, vowelsForPhones(phones))
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

  def vowelsForPhones(phones: String): String =
    phones.split(" ").filter(arpabetVowels contains _).mkString(" ")

  def consonantsForPhones(phones: String): String =
    phones.split(" ").filter(arpabetConsonants contains _).mkString(" ")

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
   * Find all homophonous words for w
   * @param w the word used for the homophone query
   * @return a Vector of valid homophones for w
   */
  def homophonesByWord(w: String): Vector[String] = {
    val homophones = for {
      p <- phonesForWord(w)
      (word, p2) <- wordsWithPhones
      if p == p2
    } yield word
    homophones.distinct
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


  def wordsByPhonesStrictAssonance(phones: String): Vector[String] = {
    val v = vowelsForPhones(phones)
    val words = for ((word, v2) <- wordsWithVowels if v == v2) yield word
    words.distinct
  }

  /**
   * Strict Assonance: words with all matching vowels (number, order, and identity)
   * Find words that match a given word w by assonance
   * @param w the word used for the assonance query
   * @return a Vector of terms with valid assonance for w
   */
  def wordsByStrictAssonance(w: String): Vector[String] = {
    val words = for {
      phones <- phonesForWord(w)
      word <- wordsByPhonesStrictAssonance(phones)
    } yield word
    words.distinct
  }

  def wordsByPhonesStrictConsonance(phones: String): Vector[String] = {
    val c = consonantsForPhones(phones)
    val words = for ((word, c2) <- wordsWithConsonants if c == c2) yield word
    words.distinct
  }



    // TODO: look by cluster
  def wordsByPhonesWeakConsonance(phones: String, ratio:Double): Vector[(String, Array[String], Double)] = {
    val c = consonantsForPhones(phones).split(" ")

    val words = for {
      (word, c2) <- wordsWithConsonants
      c2Phone = c2.split(" ")
      if (c2Phone.size > 0)
      intersection = c2.split(" ").intersect(c)
      cCoverage = intersection.size.toDouble / c.size.toDouble
      c2Coverage = intersection.size.toDouble / c2Phone.size.toDouble
      avg = (cCoverage + c2Coverage) / 2.0

      if (avg >= ratio)

    } yield (word, intersection, avg)


    words.distinct.sortBy(- _._3)
  }

  /**
   * Strict Consonance: words with all matching consonants (number, order, and identity)
   * Find words that match a given word w by consonance
   * @param w the word used for the consonance query
   * @return a Vector of terms with valid consonance for w
   */
  def wordsByStrictConsonance(w: String): Vector[String] = {
    val words = for {
      phones <- phonesForWord(w)
      word <- wordsByPhonesStrictConsonance(phones)
    } yield word
    words.distinct
  }

  /**
   * Weak Consonance: words which match any of the consonants in the given word.
   * Find all the words, f,  which match any of the consonants in a given word w, in any order
   * Once found, the average of the ratio of w which is matched and of f which is matched is calculated.
   * If that average is above the threshold, f is returned along with the overlapping consonants and the average.
   * @param w the word used for the consonance query
   * @param ratio the average matching ratio threshold
   * @return a Vector of (word, overlapping consonants, the average-overlap-ratio)
   */
  def wordsByWeakConsonance(w:String, ratio:Double): Vector[(String, Array[String], Double)] = {

    assert (ratio >= 0.25)

    val words = for {
      phones <- phonesForWord(w)
      (word, int, ratioMatched) <- wordsByPhonesWeakConsonance(phones, ratio)
    } yield (word, int, ratioMatched)
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

  def main (args:Array[String]) {
    val d = new CmuDict

    val w1 = "pest"
    println ("w1: " + w1)

    val p1 = d.phonesForWord(w1)
    println ("p1: " + p1.mkString(", "))

    val sc1 = d.wordsByStrictConsonance(w1)
    println ("\nstrict consonance matches for " + w1 + ": " + sc1.mkString(", "))

    val wc1 = d.wordsByWeakConsonance(w1, 0.7)
    println ("\nweak consonance matches for " + w1 + ": ")
    for (w <- wc1) {
     println("(" + w._1 + ", " + w._2.mkString("(",", ", ") ") + w._3 + ")")
    }





  }

}
