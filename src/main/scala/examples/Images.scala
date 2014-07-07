
package rea.examples

import rea._
import P._

object Images {

	val winSize = Vec(640, 360)
	val win = Win(size = winSize)
	val blackWin = Win(size = winSize, bkg = Color(0))

	object LoadAndDisplay {
		def main = win.setup {
			val img = Image(getClass.getResource("/meteora-small.jpg").getPath())
			img.display(0, 0)
			img.display(0, winSize.y/2, img.width/2, img.height/2)			
		}.run
	}

	object BackgroundImage {
		def setup = Image(getClass.getResource("/meteora-small.jpg").getPath())

		def draw(img: Image) = {
			Get.saw(0, 1, 0, winSize.y).map(y => { img.display; line(0, y, winSize.x, y)})
		}		

		def main = win.setup(stroke(226, 204, 0)).run(setup, draw)
	}

	object Transparency {
		def setup = Image(getClass.getResource("/meteora-small.jpg").getPath())

		def draw(img: Image) = {
			val easing = 0.05f

			def go(off: Float, mouse: Float): Float = {
				val dx = (mouse - img.width/2) - off
				off + dx * easing
			}

			val offset = Get.move(mouseX, 0.0f, go)

			offset.map(x => { 
				img.display
				tint(255, 127)
				img.display(x, 0)
			})
		}

		def main = win.run(setup, draw)
	}

	object CreateImage {

		def setup = {
			val img = Image(230, 230, Argb)
			for (i <- 0 until img.pixels.length) {
				val a = map(i, 0, img.pixels.length, 255, 0)
    			img.pixels(i) = Color(0, 153, 204, a).toProc
			}
			img
		}

		def draw(img: Image) = {
			mouse.map(v => {
				img.display(90, 80)
				img.display(v)
			})
		}

		def main = blackWin.run(setup, draw)
	}

	object Pointilism {
		def setup = Image(getClass.getResource("/meteora-small.jpg").getPath())

		def draw(img: Image) = {
			def mkPoint(v: Vec, rad: Float, alp: Float) {
				val c = img.get(v.x.toInt, v.y.toInt)
				noStroke
				fill(c.copy(alpha = alp))
				circle(v, rad)
			}

			Get.lift(mkPoint, winSize.rnd, Get.rnd(2, 20), Get.rnd(255))
		}

		def main = win.copy(clear = false).run(setup, draw)
	}

}