package rea.examples

import rea._
import P._

object Data {

	val winSize = Vec(640, 360)
	val width = winSize.x
	val height = winSize.y

	val blackWin = Win(size = winSize, bkg = Color(0))

	object Variables {

		def main = blackWin.setup {
			stroke(153)
			strokeWeight(4)

			strokeCapSquare

			var a = 50
			var b = 120.0f
			var c = 180

			line(a, b, a+c, b)
			line(a, b+10, a+c, b+10)
			line(a, b+20, a+c, b+20)
			line(a, b+30, a+c, b+30)

			a = a + c
			b = height-b

			line(a, b, a+c, b)
			line(a, b+10, a+c, b+10)
			line(a, b+20, a+c, b+20)
			line(a, b+30, a+c, b+30)

			a = a + c
			b = height-b

			line(a, b, a+c, b)
			line(a, b+10, a+c, b+10)
			line(a, b+20, a+c, b+20)
			line(a, b+30, a+c, b+30)
		}.run

	}

	object IntegerFloats {
		def main = Win(size = winSize, bkg = Color(0), frameRate = 30).setup(stroke(255)).run {
			Get.lift(display, Get.saw(0, 1, 0, width), Get.saw(0, 0.2f, 0, width))
		}

		def display(a: Float, b: Float) {
			line(a, 0, a, height/2);
  			line(b, height/2, b, height);
		}
	}

	object TrueFalse {
		def main = blackWin.setup {
			stroke(255)
			val d = 20.0f
			val middle = width / 2

			for (i <- d to width by d) {
				if (i < middle) {
					// Vertical line
					line(i, d, i, height-d)
				} else {
					// Horizontal line
    				line(middle, i - middle + d, width-d, i - middle + d)
				}
			}
		}.run
	}

	object CharactersStrings {
		def main = blackWin.run {
		  mousePressed.react(println)
		}
	}


}