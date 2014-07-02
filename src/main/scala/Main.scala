import processing.core._

object R {
	import P._

	def draw(c: Color, pos: Vec, rad: Float) {
		Shape.ellipse(pos, Vec(rad, rad))		
	}
}

object Main extends App {	
	import P._
	import Get._
	
	val f = Font.create("Arial", 20)
	val res = mouse.map(x => {fill(Color.black); text("Hello!", x)})

	// Win(fonts = Seq(f)).run(res)
	SolarDemo.run
}


object FibsDemo {
	import Get._

	lazy val fibs: Get[Int] = fsm(fibs, (0, 1), (p: (Int, Int), a: Int) => (p._2, a + p._2), (_: (Int, Int))._1)

	def run() {
		println(fibs.toList(15))
	}
}

object SpringDemo {
	import Get._

	case class B(pos: Double, vel: Double) {
		def move(force: => Get[Double]): Get[Double] = {
			val v = Get.move(force, vel, (_: Double) + (_:Double))
			val p = Get.move(v, pos, (_: Double) + (_:Double))
			p
		}
	}

	def go(to: Double, from: Double) = 0.1 * Math.signum(to - from)

	lazy val body = B(1, 0).move(force)
	lazy val force: Get[Double] = body.map(go(0, _)) 

	def run {

		body.toList(200).foreach(println)
	}

}


