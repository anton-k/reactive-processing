package rea.examples

import processing.core._
import P._


case class Circle(center: Vec, rad: Float)

object SolarDemo {
	def run = Win().run(solar(Vec(250, 250)).map(_.draw))

	def planet(center: Vec, color: Color, rad: Float, pos: Vec, speed: Float): Get[Planet] = 
		(pos -> Vec(0, speed)).map(x => Planet(color, Circle(x.fromPolar + center, rad)))

	def solar(center: Vec): Get[Solar] = 
		Get.sequence(Get.lift(planet, 
			Get.const(center),
			Color.rnd(), 
			Get.normalRnd(40, 10), 
			Vec(Get.lin(90, 20), Get.rnd()), 
			Get.normalRnd(0.005f, 0.002f)).toList(4)).map(Solar(center, _))
}

case class Planet(color: Color, shape: Circle) {
	def draw = { 
		fill(color)
		circle(shape.center, shape.rad)
	}
}

case class Solar(center: Vec, planets: List[Planet]) {
	def draw = {
		planets.foreach(x => x.draw)	
		fill(Color(0.7f, 0.7f, 0.3f))
		circle(center, 70)		
	}
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


