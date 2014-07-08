
package rea.examples

import rea._
import P._

object Drawing {
	val winSize = Vec(640, 360)
	val win = Win(size = winSize, clear = false, bkg = Color(102))

	object ContinuousLines {
		def draw(p: Boolean, a: Vec, b: Vec) {
			if (p) {
				line(a, b)
			}				
		}

		def main = win.setup(stroke(255)).run {
			Get.lift(draw, mousePressed.map(x => true).getOrElse(false), mouse, pmouse)
		}
	}

	object Patterns {
		def variableEllipse(a: Vec, b: Vec) {
			val speed = a.l1(b)
			stroke(speed)
			circle(a, speed)
		}

		def main = win.run {
			Get.lift(variableEllipse, mouse, pmouse)
		}
	}

}