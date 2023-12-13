
import scala.io.{Source, StdIn}
import scala.io.StdIn.readInt

import scala.collection.immutable.ListMap



object MyApp extends App {


  val mapdata = readFile("data.txt")
  println(mapdata)



  val actionMap = Map[Int, () => Boolean](1 -> handleOne, 2 -> handleTwo, 3 -> handleThree, 4 -> handleFour, 5 -> handleFive, 6 -> handleSix, 7 -> handleSeven)

  var opt = 0
  do {
    opt = readOption
  } while (menu(opt))

  def readOption: Int = {
    println(
      """|Please select one of the following:
        | 1 - Get current price of each food
        | 2 - Get highest and lowest prices of each food
        | 3 - Get median price for each food
        | 4 - Get the food that has risen in price the most over the last 6 months
        | 5 - Compare the average price for two foods over a two year period
        | 6 - Create a food basket
        | 7 - Exit Application
         |""".stripMargin
    )
    readInt()
  }

  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, This command is not recognised")
        true
    }
  }

  def handleOne(): Boolean = {
    val prices = currentPrice()
    prices.foreach { case (item, priceList) =>
    val currentPrice = priceList.headOption.get
      println(s"$item currently costs : $currentPrice p")
    }
    true
  }

  def handleTwo(): Boolean = {
    val prices = currentPrice()
    prices.foreach { case (item, priceList) =>
    val highestPrice = priceList.maxOption.get
    val lowestPrice = priceList.minOption.get
    println(s"$item Highest Price is : $highestPrice and the lowest price is : $lowestPrice ")
    }
    true
  }

  def handleThree(): Boolean = {
    val prices = currentPrice()
    prices.foreach { case (item, priceList) =>
    val medianPrice = calculateMedian(priceList)
    println(s"$item Median price over the last 24 months is : $medianPrice")}
    true
  }

  def handleFour(): Boolean = {
    val prices = currentPrice()

    var maxRise = Double.MinValue
    var foodWithLargestPriceRise = ""

    prices.foreach { case (item, priceList) =>
      val foodPrice = prices.getOrElse(item, priceList)

      val (earlierPrices, laterPrices) = foodPrice.splitAt(foodPrice.length - 12)
      val earlierAverage = calculateAverage(earlierPrices)
      val laterAverage = calculateAverage(laterPrices)
      val rise = laterAverage.toDouble - earlierAverage.toDouble

      if (rise > maxRise) {
        maxRise = rise
        foodWithLargestPriceRise = item
      }

    }
    println(s"the food with the largest price rise is : $foodWithLargestPriceRise")
    true
  }

  def handleFive(): Boolean = {
    val prices = currentPrice()

    println("Please enter name of a food:")
    val item1 = StdIn.readLine()

    println("please enter the name of another food:")
    val item2 = StdIn.readLine()

    val item1Prices = prices.getOrElse(item1, List())
    val item2Prices = prices.getOrElse(item2, List())

    val item1average = calculateAverage(item1Prices)
    val item2average = calculateAverage(item2Prices)

    println(s"Average Price of $item1 is : $item1average")
    println(s"Average Price of $item2 is : $item2average")

    true
  }

  def handleSix(): Boolean = {
    val prices = currentPrice()

    println("Please enter your food basket formatted as 'FOOD' 'Amount' in KGs or Litres seperate each item and amount using a space")
    val input = StdIn.readLine()
    val basket = input.split("\\s+").grouped(2).map { case Array(item, amount) => item -> amount.toDouble}.toMap

    val totalValue = calculateBasketValue(basket, prices)

    println(s"the total value of your food basket is $totalValue p")
    true
  }

  def handleSeven(): Boolean = {
    println("Selected Quit")
    false
  }


  def readFile(filename: String): Map[String, List[Int]] = {
  var mapBuffer: Map[String, List[Int]] = Map()
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        val splitline = line.split(",").map(_.trim).toList
        mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.map(_.toInt))
      }
    } catch {
      case ex: Exception => println("Sorry, an exception occurred")
    }
    mapBuffer
  }

  def currentPrice(): Map[String, List[Int]] = {
    ListMap(mapdata.toSeq: _*)
  }


  def calculateMedian(priceList: List[Int]): String = {
    val sortedPrices = priceList.sorted
    val length = sortedPrices.length
    if (length % 2 == 0) {
      val middle1 = sortedPrices((length - 1) / 2)
      val middle2 = sortedPrices(length / 2)
      ((middle1 + middle2) / 2).toString
    } else {
      sortedPrices(length / 2).toString
    }
  }

  def calculateAverage(priceList: List[Int]): String = {
    val sum = priceList.sum
    val average = sum.toDouble / priceList.length
    average.toString
  }

  def calculateBasketValue(basket: Map[String, Double], prices: Map[String, List[Int]]): Double = {
    val validBasket = basket.filter { case (item, _) => prices.contains(item)}

    val totalValue = validBasket.foldLeft(0.0) { case (acc, (item, amount)) =>
      val priceList = prices(item)
      val currentPrice = priceList.head
      acc + amount * currentPrice
    }
    totalValue
  }

}

