
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
