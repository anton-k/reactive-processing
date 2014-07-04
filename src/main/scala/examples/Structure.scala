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
		def main = Win(size = Vec(640, 360), bkg = Color(204, 153, 0)).run
	}

	object Coordinates {
		val winSize = Vec(640, 360)
		val bkgColor = Color(0)	

		def main = Win(size = winSize, bkg = bkgColor).setup {
			noFill
			stroke(Color(255))
			point(winSize / 2)
			point(winSize * Vec(0.5f, 0.25f))

			stroke(Color(0, 153, 255))
			line(Vec(0, winSize.y * 0.33f), Vec(winSize.x, winSize.y * 0.33f))

			stroke(Color(255, 153, 0))
			rect(winSize * Vec(0.25f, 0.1f), winSize * Vec(0.5f, 0.8f))
		} .run	

	}

	object WidthHeight {
		val winSize = Vec(640, 360)				

		def main = Win(size = winSize, bkg = Color(127)).setup {
			for (i <- 1 to winSize.y.toInt by 20) {
				fill(129, 206, 15)
				rect(0, i, winSize.x, 10)
				fill(255)
				rect(i, 0, 10, winSize.y)
			}
		}.run

	}

	object SetupDraw {
		val winSize = Vec(640, 360)

		def saw(s0: Float, ds: Float, min: Float, max: Float) = Get.iter(s0, (s: Float) => 
			if (s > max) min
			else if (s < min) max
			else s + ds
		)

		val ys = saw(100, -1, 0, winSize.y)

		def main = Win(size = winSize, frameRate = 30, bkg = Color(0)).setup(stroke(255)).run {
			ys.map(y => line(0, y, winSize.x, y))
		}
	}

	object NoLoop {
		val winSize = Vec(640, 360)

		def saw(s0: Float, ds: Float, min: Float, max: Float) = Get.iter(s0, (s: Float) => 
			if (s > max) min
			else if (s < min) max
			else s + ds
		)

		val ys = saw(100, -1, 0, winSize.y)

		def main = Win(size = winSize, frameRate = 30, bkg = Color(0)).setup{stroke(255); noLoop }.run {
			ys.map(y => line(0, y, winSize.x, y))
		}
	}

	object Loop {
		// The original example doesn't work due to different organisation
		// of the animation drawing functions. In this library we can not
		// know if mouse was pressed if we don't invoke draw (wich is prohibited by noLoop)
		//
		// But we can simulate the example like this: We add zeroes to the line width and only
		// when we press the mouse we start adding (-1)'s. The saw function now takes not
		// the constant displacment but the stream of displacements. Otherwise it's the same.

		val winSize = Vec(640, 360)

		def saw(s0: Float, deltas: Get[Float], min: Float, max: Float) = Get.fsm(deltas, s0, (s: Float, ds: Float) => 
			if (s > max) min
			else if (s < min) max
			else s + ds
		, (x: Float) => x) 

		val ys = saw(100, mousePressed.map(x => -1.toFloat).step(0), 0, winSize.y)

		def main = Win(size = winSize, frameRate = 30, bkg = Color(0)).setup(stroke(255)).run {
			ys.map(y => line(0, y, winSize.x, y))
		}
	}


	object Redraw {
		// Again the original example doesn't work. But we can simulate it.
		// But know we add (-1)'s only when mouse is pressed.

		val winSize = Vec(640, 360)

		def saw(s0: Float, deltas: Get[Float], min: Float, max: Float) = Get.fsm(deltas, s0, (s: Float, ds: Float) => 
			if (s > max) min
			else if (s < min) max
			else s + ds
		, (x: Float) => x)

		val ys = saw(100, mousePressed.map(x => -1.toFloat).getOrElse(0), 0, winSize.y)

		def main = Win(size = winSize, frameRate = 30, bkg = Color(0)).setup(stroke(255)).run {
			ys.map(y => line(0, y, winSize.x, y))
		}
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

		def main = Win(size = winSize, bkg = Color(51)).setup {
			drawTarget(width*0.25f, height*0.4f, 200, 4)
			drawTarget(width*0.5f, height*0.5f, 300, 10)
			drawTarget(width*0.75f, height*0.3f, 120, 6)
		}.run
	}

	object Recursion {				

		val winSize = Vec(640, 360)

		def main = Win(size = winSize).setup {
			drawCircle(winSize.x / 2, 280, 6) 
		}.run

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

	object BallFollowMouse {
		def main = Win().run {
			mouse.map(x => { fill(0); circle(x, 50) })
		}		
	}

	object BallFollowOnClick {
		def main = Win().run {
			mousePressed.step(Vec(250, 250)).map(x => { fill(0); circle(x, 50) })
		}		
	}


}

