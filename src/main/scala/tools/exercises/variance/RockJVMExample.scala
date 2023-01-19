package fr.maxime.binandco
package tools.exercises.variance

private object RockJVMExample extends App {

  private trait Food {
    def calories: Int
  }

  private trait Sandwich extends Food {
    def calories: Int

    def isToasted: Boolean
  }

  private val basicFood: Food = new Food {
    override def calories: Int = 50
  }

  private val hamSandwich: Sandwich = new Sandwich {
    override def calories: Int = 100

    override def isToasted: Boolean = false
  }

  private final case class Box[+A](valueBefore: Option[A] = Option.empty, value: A) {
    val boxType: String = getBoxType

    private def getBoxType: String = {
      value match {
        case _: Sandwich => "Sandwich"
        case _: Food => "Food"
      }
    }

    def setNewValue[A1 >: A](value: A1): Box[A1] = {
      Box(Some(this.value), value)
    }
  }

  private val sandwichBox: Box[Sandwich] = Box(value = hamSandwich)

  private def printBoxInfo(box: Box[Food]): Unit = {
    println(s"type: ${box.boxType}\n - before : ${
      box.valueBefore.getOrElse(new Food {
        def calories: Int = 0
      }).calories
    } Kcal | now : ${box.value.calories} Kcal")
  }

  private val foodBox: Box[Food] = Box(value = basicFood: Food)
  printBoxInfo(foodBox)

  private val newFoodBox = foodBox.setNewValue(hamSandwich: Sandwich)
  printBoxInfo(newFoodBox)

  private def eatFoodInBox(box: Box[Food]): Unit = {

    println("\nAnalyzing...")

    // 1st way:
    if (box.value.isInstanceOf[Sandwich]) {
      val sBox = box.asInstanceOf[Box[Sandwich]]
      val sandwich = box.value.asInstanceOf[Sandwich]
      println(s"Oh this is a Sandwich," +
        s"\n - Is it Toasted ? ${sBox.value.isToasted} !" +
        s"\n - How many calories ? ${sandwich.calories} calories !")
    }
    else if (box.value.isInstanceOf[Food]) {
      println(s"This is a normal food," +
        s"\n - How many calories ? ${box.value.calories} calories !")
    }

    // 2nd way:
    box.value match {
      case sandwich: Sandwich =>
        val sBox = box.asInstanceOf[Box[Sandwich]]
        println(s"Oh this is a Sandwich," +
          s"\n - Is it Toasted ? ${sBox.value.isToasted} !" +
          s"\n - How many calories ? ${sandwich.calories} calories !")
      case food: Food =>
        println(s"This is a normal food," +
          s"\n - How many calories ? ${food.calories} calories !")
    }

  }

  eatFoodInBox(sandwichBox)
  eatFoodInBox(foodBox)

}
