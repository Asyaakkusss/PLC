// Kaia Kanj (kmk233) and Asya Akkus (aya29)

import kotlin.collections.List
import java.io.File

interface CodeParser {
    fun parseCode (filename: String): List<Node>
}

open class Node (var text: String) {
    open fun toHTML(): String {
        return text
    }
}

class MarkdownNode: Node {

    var type: Int = 0

    constructor (text:String) : super(text)

    override fun toHTML(): String {
        
            return when (type) {
                1 -> "<h3>$text</h3>"  
                2 -> "<h2>$text</h2>"
                3 -> "<h1>$text</h1>"  
                4 -> "<b>$text</b>"
                5 -> "<i>$text</i>"
                6 -> "<blockquote>$text</blockquote>" 
                7 -> "<hr />"                
                else -> text
            }
    }
}


// MarkdownParser implementing CodeParser
class MarkdownParser : CodeParser {
    override fun parseCode(filename: String): List<Node> {
        val lines = File(filename).readLines()
        val nodes = mutableListOf<Node>()

        for (line in lines) {
            val trimmed = line.trim() // trim leading/trailing whitespace
            when {
                // header level 3
                trimmed.matches(Regex("""^#{3}\s+(.+)$""")) -> {
                    val content = trimmed.removePrefix("### ").trim()
                    val node = MarkdownNode(content)
                    node.type = 1
                    nodes.add(node)
                }
                // header level 2
                trimmed.matches(Regex("""^#{2}\s+(.+)$""")) -> {
                    val content = trimmed.removePrefix("## ").trim()
                    val node = MarkdownNode(content)
                    node.type = 2
                    nodes.add(node)
                }
                // header level 1
                trimmed.matches(Regex("""^#{1}\s+(.+)$""")) -> {
                    val content = trimmed.removePrefix("# ").trim()
                    val node = MarkdownNode(content)
                    node.type = 3
                    nodes.add(node)
                }
                // bold
                trimmed.matches(Regex("""^\*\*(.+)\*\*$""")) -> {
                    val content = trimmed.removeSurrounding("**").trim()
                    val node = MarkdownNode(content)
                    node.type = 4
                    nodes.add(node)
                }
                // italic
                trimmed.matches(Regex("""^\*(.+)\*$""")) -> {
                    val content = trimmed.removeSurrounding("*").trim()
                    val node = MarkdownNode(content)
                    node.type = 5
                    nodes.add(node)
                }
                // block quote
                trimmed.matches(Regex("""^>\s+(.*)$""")) -> {
                    val content = trimmed.removePrefix("> ").trim()
                    val node = MarkdownNode(content)
                    node.type = 6
                    nodes.add(node)
                }
                // horizontal line
                trimmed.matches(Regex("""^---$""")) -> { 
                    val node = MarkdownNode("")
                    node.type = 7
                    nodes.add(node)
                }
                // regular text
                trimmed.isNotEmpty() -> { 
                    val node = MarkdownNode(trimmed)
                    node.type = 0
                    nodes.add(node)                    
                }
            }
        }

        return nodes
    }
}

// main function to read readme.md and write readme.html
fun main() {
    val parser = MarkdownParser()
    val nodes = parser.parseCode("readme.md") // update name based on file being tested

    val htmlOutput = buildString {
        for (node in nodes) {
            appendLine(node.toHTML())
        }
    }

    File("readme.html").writeText(htmlOutput) // update name based on file being written
    println("readme.html created.") // update name based on file being written
}