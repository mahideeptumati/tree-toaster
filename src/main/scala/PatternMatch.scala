import PatternMatch.patternMatches

import scala.collection.mutable.ListBuffer

class Node(var data: Int,
           var left: Node,
           var right: Node) {
  def this(data: Int) {
    this(data, null, null);
  }
}

class Tree() {


  def helper(node: Node, pattern: Array[Int], index: Int): Boolean = {
    //println(patternMatches)

    var newIndex = index
    if (index >= pattern.length || node.data != pattern(index)) {
      newIndex = 0
    }
    if (pattern.length ==1 ){
      if(node.data == pattern(newIndex))
        patternMatches += true;
    }
    else if (newIndex == pattern.length - 1 || (node.left == null && node.right == null && newIndex == pattern.length - 1)) {
      patternMatches += true;
      newIndex = 0
      //return true
    }

    else if (node.data == pattern(newIndex) && newIndex+1<= pattern.length) newIndex += 1

    var result = false

    if (node.left != null) {
      result = helper(node.left, pattern, newIndex )
      if (result) {
        //newIndex=0
        //return helper(node.data)
      }
    }
    if (node.right != null) {
      result = helper(node.right, pattern, newIndex )
      if (result) {
        //newIndex=0
        //return true
      }
    }
    return false
  }

  def findPath(node: Node, pattern: Array[Int]): Boolean = {
    //  println(pattern(0))
    if (node == null) {
      return pattern.length == 0;
    }
    return helper(node, pattern, 0);
  }
}

object PatternMatch {


  var patternMatches = new ListBuffer[Boolean]()

  def main(args: Array[String]): Unit = {

    //                0
    //         1              0
    //    0        1       0
    //       1   0   0

    var root = new Node(0)
    root.left = new Node(1)
    root.right = new Node(0)
    root.left.left = new Node(0)
    root.left.right = new Node(1)
    root.right.left = null
    root.right.right = new Node(0)
    root.left.left.right = new Node(1)
    root.left.right.left = new Node(0)
    root.left.right.right = new Node(0)

    // Display result
    var pattern1 = Array(1,0);
    var pattern2 = Array(1,0,0);
    var pattern3 = Array(0);
    var pattern4 = Array(1,0);
    var pattern5 = Array(1,0);

    new Tree().findPath(root, pattern1)
    println(patternMatches)
  }
}