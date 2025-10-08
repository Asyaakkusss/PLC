// Kaia Kanj (kmk233) and Asya Akkus (aya29)

import kotlin.collections.List

interface CodeParser {
    fun parseCode (filename: String): List<Node>
}


open class Node (var text: String) {
    open fun toHTML(): String {
        return text
    }
}

class MarkdownNode: Node {
    override fun toHTML(): String {
        return when (type) {
            // TODO: implement markdown to HTML conversion
            // I was thinking of using a switch here/the kotlin equivalent (not sure what it's techinally called in kotlin)
            // I assigned numbers to each markdown type in MarkdownParser. Change however you want though
            else -> text // placeholder
        }
    }
}

// MarkdownParser implementing CodeParser
// TODO: Can you please check my regex? I am not 100% sure if it's correct.
class MarkdownParser : CodeParser {
    override fun parseCode(filename: String): List<Node> {
        val lines = File(filename).readLines()
        val nodes = mutableListOf<Node>()

        for (line in lines) {
            val trimmed = line.trim() // trim leading/trailing whitespace
            when {
                // header level 3
                trimmed.matches(Regex("^#{3}\s+(.+)$")) -> {
                    val content = trimmed.removePrefix("### ").trim()
                    nodes.add(MarkdownNode(content, 1))
                }
                // header level 2
                trimmed.matches(Regex("^#{2}\s+(.+)$")) -> {
                    val content = trimmed.removePrefix("## ").trim()
                    nodes.add(MarkdownNode(content, 2))
                }
                // header level 1
                trimmed.matches(Regex("^#{1}\s+(.+)$")) -> {
                    val content = trimmed.removePrefix("# ").trim()
                    nodes.add(MarkdownNode(content, 3))
                }
                // bold
                trimmed.matches(Regex("^\*\*(.+)\*\*$")) -> {
                    val content = trimmed.removeSurrounding("**").trim()
                    nodes.add(MarkdownNode(content, 4))
                }
                // italic
                trimmed.matches(Regex("^\*(.+)\*$")) -> {
                    val content = trimmed.removeSurrounding("*").trim()
                    nodes.add(MarkdownNode(content, 5))
                }
                // block quote
                trimmed.matches(Regex("^>\s+(.*)$")) -> {
                    val content = trimmed.removePrefix("> ").trim()
                    nodes.add(MarkdownNode(content, 6))
                }
                // horizontal line
                trimmed.matches(Regex("^---$")) -> { //
                    nodes.add(MarkdownNode("", 7))
                }
                // regular text
                trimmed.isNotEmpty() -> { 
                    nodes.add(MarkdownNode(trimmed, 0))
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