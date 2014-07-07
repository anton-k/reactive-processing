
package rea.examples

import rea._
import P._

object Arrays {

	val winSize = Vec(640, 360)
	val whiteWin = Win(size = winSize, bkg = Color(255))
	val blackWin = Win(size = winSize, bkg = Color(0))
	
	val width = winSize.x
	val height = winSize.y

	object Arrays {

		val cosWave: Array[Float] = (0 until width.toInt).map(i =>abs(cos(map(i, 0, width, 0, 0.5f)))).toArray

		def main = whiteWin.setup {
			def go(myStroke: Float => Unit, myLine: Float => Unit) = (0 until width.toInt by 2).foreach { i =>
				myStroke(255 * cosWave(i))
				myLine(i.toFloat)
			}

			def verLine(y1: Float, y2: Float)(i: Float) = line(i, y1, i, y2)
			def line1 = verLine(0, height/3) _
			def line2 = verLine(height/3, 2 * height/ 3) _
			def line3 = verLine(2 * height / 3, height) _

			go(stroke, line1)			
			go(x => stroke(x / 4), line2)
			go(x => stroke(255 - x), line3)
		}.run
	}

	object Arrays2D {
		def fromCenter(a: Vec) = (a - winSize/2).magnitude		
		val maxDistance = fromCenter(winSize)
		val distances =
			(0 until height.toInt).map { (y: Int) =>
				(0 until width.toInt).map {	(x: Int) =>				
					fromCenter(Vec(x, y)) / maxDistance
				}.toArray
			}.toArray
		

		val spacer = 10

		def main = blackWin.setup {

			for (y <- 0 until height.toInt by spacer) {
				for (x <- 0 until width.toInt by spacer) {
					stroke(255 * distances(y)(x))
					point(x + spacer.toFloat/2, y + spacer.toFloat/2)				
				}
			}

		}.run
	}

	object ArrayObject {

		case class Module(xOffset: Float, yOffset: Float, x: Float, y: Float, xDirection: Float, yDirection: Float, speed: Float, unit: Float) {
			def move: Get[Module] = modules(this)

			def draw {
				fill(255)
				circle(xOffset + x, yOffset + y, 3)
			}
		}

		def modules(m: Module): Get[Module] = {
			def update(a: Module): Module = {
				var x = a.x
				var y = a.y
				var xDirection = a.xDirection
				var yDirection = a.yDirection

				x = x + a.speed * xDirection
				if (x >= a.unit || x <= 0) {
					xDirection *= -1
					x = x + xDirection
					y = y + yDirection
				}
				if (y >= unit || y <= 0) {
					yDirection *= -1
					y = y + yDirection
				}

				a.copy(x = x, y = y, xDirection = xDirection, yDirection = yDirection)
			}

			Get.iter(m, update)
		}

		val unit = 40.0f
		val highCount = (height / unit).toInt
		val wideCount = (width / unit).toInt

		val speeds = Get.rnd(0.05f, 0.8f).toIterator		

		val ms = Get.sequence {
			(for { 
				y <- 0 until highCount
				x <- 0 until wideCount 
			} yield Module(x.toFloat * unit, y.toFloat * unit, unit/2, unit/2, 1, 1, speeds.next(), unit).move).toList
		}

		def main = blackWin.setup(noStroke).run {
			ms.map(_.foreach(_.draw))
		}
	}
}