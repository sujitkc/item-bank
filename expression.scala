object DB {
  var db : scala.collection.mutable.Map[String, Set[String]] =
    new scala.collection.mutable.HashMap[String, Set[String]]()
  
/*   db("python") = Set("item1", "item2");
  db("list") = Set("item1", "item3", "item4")
 */
  val bufferedSource = io.Source.fromFile("db.csv")
  for (line <- bufferedSource.getLines) {
    val cols = line.split(",").map(_.trim)
    db(cols(0)) = cols.slice(1, cols.length - 1).toSet
  }
  bufferedSource.close
  
  def items(label : String) = db(label)
  def allitems() : Set[String] = {
    var all : scala.collection.mutable.Set[String] =
      new scala.collection.mutable.HashSet[String]();
    for(k <- db.keys) {
      all = all ++ db(k)
    };
    all.toSet
  }

}

abstract class Expression {
  def items() : Set[String];
}

class LabelExpression (
  label : String  
) extends Expression {
  def items() : Set[String] = DB.items(label)
}

class OrExpression (
  left : Expression,
  right : Expression
) extends Expression {
  def items() : Set[String] = left.items() ++ right.items()
}

class AndExpression (
  left : Expression,
  right : Expression
) extends Expression {
  def items() : Set[String] = left.items() & right.items()
}

class NotExpression (
  e : Expression,
) extends Expression {
  def items() : Set[String] = DB.allitems() &~ e.items()
}

object Main {
  def main(args: Array[String]) = {
    val l1 = new LabelExpression("Python")
    val l2 = new LabelExpression("list")
    val l3 = new OrExpression(l1, l2)
    println(l3.items())
    val l4 = new AndExpression(l1, l2)
    println(l4.items())
    val l5 = new NotExpression(l1)
    println(l5.items())
  }
}