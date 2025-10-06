import kotlin.collections.List




interface CodeParser {
    fun parseCode (filename: String): List<Node>
}

class MarkdownParser: CodeParser {
    fun parseCode (filename: String): List<Node> {
        return filename; 
    }
}

open class Node (var text: String) {
    open fun toHTML(): String {
        return text
    }
}

class MarkdownNode: Node {
    override fun toHTML(): String {
        
    }
}