import scala.io.Source
import java.io.File

object Try {
  def extractLabel(s : String) = {
    val l = s.length
    val pre = "%itemlabel{" 
    val lpre = pre.length
    if(s.startsWith(pre)) {
        s.substring(lpre, s.length - 1)
    }
    else ""
  }

  def extractLabelsFromFile(fname : String) = {
    val lines = Source.fromFile(fname).getLines.toList
    val labels = for(line <- lines) yield extractLabel(line)
    labels.filter(_.length != 0)
  }

  def getItemDirectories(dir: String) = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isDirectory).toList
    } else {
        List[File]()
    }
  }

  def addEntries(map : scala.collection.mutable.Map[String, List[String]], 
    fname:String, labels: List[String]) = {
    for(label <- labels) {
      if(map.contains(label)) {
        map(label) = fname :: map(label) 
      }
      else {
        map(label) = List(fname)
      }
    }
  }

  def allItems (root : String) = {
    val itemDirs = getItemDirectories(root)
    val labelItemMap = scala.collection.mutable.Map[String, List[String]]()

    for(directory <- itemDirs) {
      val itemFile = directory + "/item.tex"
      println("**" + itemFile + "**")
      val labels = extractLabelsFromFile(itemFile)
      labels.foreach(label => println(label))
      addEntries(labelItemMap, directory.getName(), labels)
    }
    labelItemMap
  }

  def main(args : Array[String]) = {
    val lines = extractLabelsFromFile("/home/sujit/IIITB/courses/resources/Python/PythonKT/item-bank/item1/item.tex")
    lines.foreach(line => println(line))
    val root = "/home/sujit/IIITB/courses/resources/Python/PythonKT/item-bank/"
    val directories = getItemDirectories(root)
    val map = allItems(root)
    println(map)
  }
}

class Expression {

}