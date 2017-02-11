import scala.util.Random

class Generator {
    var sampleCount = 10
    val random = new Random
    var samplePattern = Array("_", "_")
    val source = scala.io.Source.fromFile("./resources/nouns.txt")
    val nouns = source.getLines.toList

    def setSampleCount(value: Int) = {
        if (value > 0) {
            sampleCount = value
        } else {
            throw new IllegalArgumentException("Invalid sample count. Must be > 0");
        }
    }

    def setSamplePattern(value: Array[String]) = {
        if (value.length > 0) {
            samplePattern = value
        } else {
            throw new IllegalArgumentException("Invalid pattern")
        }
    }

    def makeSamples: Array[String] = {
        Array.fill(sampleCount) {
            if (isSingleWordPattern) {
                randomlyGenerateSuffixOrPrefixFor(word = samplePattern(0))
            } else {
                fillPattern
            }
        }
    }

    private def isSingleWordPattern: Boolean = {
        return (samplePattern.length == 1 && samplePattern(0) != "_")
    }

    private def randomlyGenerateSuffixOrPrefixFor(word: String): String = {
        val position = random.nextInt(2)
        val r = random.nextInt(nouns.length)
        val noun = nouns(r)

        if (position == 0) {
            noun.capitalize + word.capitalize
        } else {
            word.capitalize + noun.capitalize
        }
    }

    private def fillPattern: String = {
        samplePattern.reduceLeft[String] {(a, b) =>
            // Is there a way to make "_" into just _? How would that be expressed with types?
            if (a == "_" && b == "_") {
                val r1 = random.nextInt(nouns.length)
                val r2 = random.nextInt(nouns.length)
                nouns(r1).capitalize + nouns(r2).capitalize
            } else if (a == "_") {
                val i = random.nextInt(nouns.length)
                nouns(i).capitalize + b.capitalize
            } else if (b == "_") {
                val i = random.nextInt(nouns.length)
                a.capitalize + nouns(i).capitalize
            } else {
                a.capitalize + b.capitalize
            }
        }
    }
}
