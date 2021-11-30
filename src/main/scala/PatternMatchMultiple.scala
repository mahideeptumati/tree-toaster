import PatternMatchMultiple.patternMatches

import scala.collection.mutable.ListBuffer

class TreeNode(var _data: Int) {
  var data: Int = _data
  var children: List[TreeNode] = List()
}

class AST() {

  def helper(node: TreeNode, pattern: Array[Int], index: Int): Unit = {

    var newIndex = index

    if (index >= pattern.length || node.data != pattern(index)) {
      newIndex = 0
    }

    if (pattern.length == 1) {
      if (node.data == pattern(newIndex)) {
        patternMatches += true
      }
    }

    else if (newIndex == pattern.length - 1) {
      patternMatches += true;
      newIndex = 0
      //returntrue
    }

    else if (node.data == pattern(newIndex) && newIndex + 1 <= pattern.length) newIndex += 1

    var result = false

    for (child <- node.children) {
      if (child != null) {
        helper(child, pattern, newIndex)
      }
    }

    return false
  }

  def findPath(node: TreeNode, pattern: Array[Int]): Unit = {
    if (node == null) return pattern.length == 0

    helper(node, pattern, 0)
  }

}

object PatternMatchMultiple {


  var patternMatches = new ListBuffer[Boolean]()

  def main(args: Array[String]): Unit = {

    //                0
    //         1      0      1
    //    1      0  0       0   0

    var root = new TreeNode(0)
    root.children = root.children :+ (new TreeNode(1))
    root.children = root.children :+ (new TreeNode(0))
    root.children = root.children :+ (new TreeNode(1))
    root.children(0).children = root.children(0).children :+ (new TreeNode(1))
    root.children(0).children = root.children(0).children :+ (new TreeNode(0))
    root.children(1).children = root.children(2).children :+ (new TreeNode(0))
    root.children(2).children = root.children(2).children :+ (new TreeNode(0))
    root.children(2).children = root.children(2).children :+ (new TreeNode(0))
    // Display result
    var pattern1 = Array(1, 0);
    var pattern2 = Array(0, 0, 0);
    var pattern3 = Array(0, 0);
    var pattern4 = Array(0);
    var pattern5 = Array(1);

    new AST().findPath(root, pattern1)
    println(patternMatches)
  }
}
