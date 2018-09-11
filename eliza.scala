val conjugations = Map("are" -> "am",
		       "were" -> "was",
		       "i" -> "you",
		       "im" -> "you are",
		       "my" -> "your",
		       "me" -> "you",
		       "ive" -> "you've",
		       "you" -> "I",
		       "your" -> "my",
		       "myself" -> "yourself",
		       "yourself" -> "myself",
			   "am" -> "are")
			   
import scala.collection.mutable.Set

def readReplies (fileName : String) : (List[String], Map[String,Set[String]]) = {
	val file = scala.io.Source.fromFile(fileName)
	var resultMap = Map[String,Set[String]]()
	var resultArr = new scala.collection.mutable.ArrayBuffer[String]
	var firstTry = true
	
	var keywords = Array[String]()
	var answerSet = Set[String]()
	for (line <- file.getLines()) {
		if (line.contains(":")) {
			if (firstTry) {
				keywords = line.substring(0,line.length-1).split(124.toChar)
				firstTry = false
			}
			
			else {
				for (word <- keywords) {
					resultMap += (word -> answerSet)
					resultArr += word
				}
				keywords = line.substring(0,line.length-1).split(124.toChar)
				answerSet = Set[String]()
			}
		}
		
		else {
			answerSet += line
		}
	}
	for (word <- keywords) {
		resultMap += (word -> answerSet)
		resultArr += word
	}
	
	(resultArr.toList, resultMap)
}

def sanitize(input : String) : String = {
	" " + input.toLowerCase.trim.replaceAll("[^A-Za-z0-9\\s]","") + " "
}

val (keywordList , answerMap) = readReplies("replies.txt")

def findKeyword (s: String): String = {
	for (keyword <- keywordList.toArray) {
		if (s contains keyword) {
			if (keyword.split(" ").toSet subsetOf s.split(" ").toSet ) {
				return keyword
			}
		}
	}
	""
}

def main1() {
	println (">Hi! I'm Eliza, what is your prolem?")
	var cont = 0
	var last_input = ""
	while (cont == 0) {
		var input = readLine(">")
		var answer = ".....?"
		if (input == "shut up" || input == "Shut up") {
			cont += 1
		}
		else if (sanitize(last_input) == sanitize(input)) {
			answer = "Don't repeat that again, You!"
		}
		else {
			var keyword = findKeyword(sanitize(input))
			var possAns = answerMap(keyword).toArray
			answer = possAns(scala.util.Random.nextInt(possAns.length))
			if (answer.endsWith("*")) {
				var a = input.indexOf(keyword)
				var ree = input.split(keyword)
				var res = ree(1)
				var rps : Array[String] = res.split(" ")
				for (i <- 0 to rps.size -1) {
					if (conjugations contains rps(i)) {
						rps(i) = conjugations(rps(i))
					}
				}
				var rep : String = rps.mkString(" ")
				answer = answer.replace("*",  rep)
			}
		}
		last_input = input
		print (">")
		println (answer.trim)
	}
}

main1()
