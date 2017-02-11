import scala.io.StdIn.readLine

object Nounify {
    var looping = false;
    val generator = new Generator  // move to config

    def main(args: Array[String]) = {
        parseArguments(args)
        execute
    }

    def parseArguments(args: Array[String]) = {
        val SampleCount = "(\\d*)".r
        val SampleWord = "(\\D*[^_])".r
        val WildFirstWord = "[_](\\D*[^_])".r
        val WildLastWord = "(\\D*[^_])[_]".r
        val WildFirstAndLast = "[_](\\D*[^_])[_]".r

        // need more structure here.
        // pattern match the Array
        // - [count]
        // - [count] [options]
        // - [word]
        // - [word] [options]
        // - [word] [count]
        // - [word] [count] [options]
        
        for (arg <- args) {
            arg match {
                case "-l" | "--loop" => looping = true
                case "-h" | "--help" => showUsage
                // "-c | --count <value>"
                // "-w | --word <value>"
                case SampleCount(c) => setSampleCount(c.toInt)
                case WildFirstAndLast(c) => setSamplePattern("_", c, "_")
                case WildFirstWord(c) => setSamplePattern("_", c)
                case WildLastWord(c) => setSamplePattern(c, "_")
                case SampleWord(c) => setSamplePattern(c)
                case _ => println("No match")
            }
        }
    }

    def showUsage = {
        println(s"\nUsage: nounify [word] [sampleCount] [options]\n")
        println("Options:")
        // "-c | --count <value>"
        // "-w | --word <value>"
        println("-h | --help  Show how to use the program.\n")
        println("-l | --loop  Continuously show samples on a user controlled loop.\n")
    }

    def setSampleCount(value: Int) = {
        try {
            generator.setSampleCount(value)
        } catch {
            case e: Exception => println(e.getMessage)
        }
    }

    def setSamplePattern(values: String*) = {
        try {
            generator.setSamplePattern(values.toArray)
        } catch {
            case e: Exception => println(e.getMessage)
        }
    }

    def execute = {
        if (looping) {
            loopExecute
        } else {
            singleExecute
        }
    }

    private def loopExecute = {
        while(true) {
            val results = generator.makeSamples
            results.map(x => println(s" - $x"))
            println("Press ENTER to generate more samples, or press Ctrl+C to quit.")
            readLine()
        }
    }

    private def singleExecute = {
        val results = generator.makeSamples
        results.map(x => println(s" - $x"))
        println("")
    }
}
