import scala.io.Source
import java.io.File
import java.io.PrintWriter

object MakeDB {
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
      val labels = extractLabelsFromFile(itemFile)
      addEntries(labelItemMap, directory.getName(), labels)
    }
    labelItemMap
  }

  def printDB(db : scala.collection.mutable.Map[String, List[String]]) = {
    val lines = for(label <- db.keys) yield {
      label + db(label).foldLeft("")(_ + ", " + _)
    }
    for(line <- lines) println(line)
  }

  def main(args : Array[String]) = {
    val root = args(0)
    val db = allItems(root)
    printDB(db)
  }
}
