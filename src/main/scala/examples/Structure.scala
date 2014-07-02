package rea.examples

import rea._
import rea.P._


object Structure {

	// ----------------------------------------------
	// Statements and Comments. 

	object StatementsAndComments {		
		// We can set all init parameters in the case class Win
		// We set the window size to (640, 360) and background color to (204, 153, 0).
		//
		// To run the animation we invoke the method run.			
		def main = Win(size = Vec(640, 360), bkg = Color(204, 153, 0)).run()
	}

	object Coordinates {
		val winSize = Vec(640, 360)
		val bkgColor = Color(0)	

		def main = Win(size = winSize, bkg = bkgColor).run {
			noFill
			stroke(Color(255))
			point(winSize / 2)
			point(winSize * Vec(0.5f, 0.25f))

			stroke(Color(0, 153, 255))
			line(Vec(0, winSize.y * 0.33f), Vec(winSize.x, winSize.y * 0.33f))

			stroke(Color(255, 153, 0))
			rect(winSize * Vec(0.25f, 0.1f), winSize * Vec(0.5f, 0.8f))
		}	

	}

	object WidthHeight {
		val winSize = Vec(640, 360)				

		def main = Win(size = winSize, bkg = Color(127)).run {
			for (i <- 1 to winSize.y.toInt by 20) {
				fill(129, 206, 15)
				rect(0, i, winSize.x, 10)
				fill(255)
				rect(i, 0, 10, winSize.y)
			}
		}

	}

	object SetupDraw {
		val winSize = Vec(640, 360)

		def saw(s0: Float, ds: Float, min: Float, max: Float) = Get.iter(s0, (s: Float) => 
			if (s > max) min
			else if (s < min) max
			else s + ds
		)

		val ys = saw(100, -1, 0, winSize.y)

		def main = Win(size = winSize, frameRate = 30, bkg = Color(0)).run {
			ys.map(y => { stroke(255); line(0, y, winSize.x, y) }) // missing inits -- have to reStroke each time
		}
	}

	object NoLoop {
		// Missing inits
	}

	object Loop {
		// Missing inits
	}

	object Redraw {
		// Missing inits
	}

	object Functions {

		def drawTarget(xloc: Float, yloc: Float, size: Int, num: Int) {
			val grayValues = 255 / num
			val steps = size / num
			
			for (i <- 0 until num) {
				noStroke
				fill(i * grayValues)
				circle(Vec(xloc, yloc), size - i*steps)
			}
		}

		val winSize = Vec(640, 360)
		val width = winSize.x
		val height = winSize.y

		def main = Win(size = winSize, bkg = Color(51)).run {
			drawTarget(width*0.25f, height*0.4f, 200, 4)
			drawTarget(width*0.5f, height*0.5f, 300, 10)
			drawTarget(width*0.75f, height*0.3f, 120, 6)
		}
	}

	object Recursion {				

		val winSize = Vec(640, 360)

		def main = Win(size = winSize).run {
			drawCircle(winSize.x / 2, 280, 6) 
		}

		def drawCircle(x: Float, radius: Int, level: Int) {
			noStroke
			fill((126 * level).toFloat / 4)
			circle(x, winSize.y / 2, radius)
			if (level > 1) {
				drawCircle(x - radius/2, radius/2, level - 1)
				drawCircle(x + radius/2, radius/2, level - 1)
			}
		}
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

