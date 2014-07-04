
package rea.examples

import rea._
import rea.P._

object Forms {
	val winSize = Vec(640, 360)
	val grayWin = Win(size = winSize, bkg = Color(100))
	val blackWin = Win(size = winSize, bkg = Color(0))


	object PointsAndLines {

		def main = grayWin.setup{
			val d = 70
			val p1 = d
			val p2 = p1 + d
			val p3 = p2 + d
			val p4 = p3 + d

			noSmooth
			translate(Vec(140, 0))

			// Draw lines
			stroke(153)
			line(p3, p3, p2, p3)
			line(p2, p3, p2, p2)
			line(p2, p2, p3, p2)
			line(p3, p2, p3, p3)

			// Draw white points
			stroke(255);
			point(p1, p1)
			point(p1, p3) 
			point(p2, p4)
			point(p3, p1) 
			point(p4, p2)
			point(p4, p4)
		}.run
	}

	object ShapePrimitives {

		def main = grayWin.setup{

			noStroke

			fill(204)
			triangle(18, 18, 18, 360, 81, 360)

			fill(102)
			rect(81, 81, 63, 63)

			fill(204)
			quad(189, 18, 216, 18, 216, 360, 144, 360)

			fill(255)
			ellipse(252, 144, 72, 72)

			fill(204)
			triangle(288, 18, 351, 360, 288, 360)

			fill(255)
			arc(479, 300, 280, 280, 0.5f, 1)
		}.run
	}

	object PieChart {

		def main = grayWin.setup{

			val angels = Seq[Float](30, 10, 45, 35, 60, 38, 75, 67)

			noStroke
			pieChart(300, angels)
		}.run

		def pieChart(diameter: Float, data: Seq[Float]) {
			var lastAngle: Float = 0
			for (i <- 0 until data.length) {
				val gray = map(i, 0, data.length, 0, 255)
				val nextAngle = lastAngle + data(i) / 360
				fill(gray)
				arc(winSize.x / 2, winSize.y / 2, diameter, diameter, lastAngle, nextAngle)
				lastAngle = nextAngle
			}
		}
	}

	object RegularPolygons {

		def main = grayWin.run {
			Get.lin(0, 0.1f).map(display)
		}

		def display(frames: Float) {
			def poly(speed: Float, x: Float, rad: Float, n: Int) = 
				inSpace(winSize * Vec(x, 0.5f), frames / speed) {
					polygon(Vec(0, 0), rad, n)
				}

			poly(200, 0.2f, 82, 3)
			poly(50,  0.5f, 80, 20)
			poly(-100, 0.8f, 70, 7)			
		}

		def polygon(c: Vec, radius: Float, npoints: Int) = Contour(
			((Vec(radius, 0) -> Vec(0, 1 / npoints.toFloat)).fromPolar + c).toList(npoints)
		).close
	}

	object Star {

		def main = grayWin.run {
			Get.lin(0, 0.1f).map(display)
		}

		def display(frames: Float) {
			def stary(speed: Float, x: Float, rad1: Float, rad2: Float, n: Int) = 
				inSpace(winSize * Vec(x, 0.5f), frames / speed) {
					star(Vec(0, 0), rad1, rad2, n)
				}

			stary(200, 0.2f, 5, 70, 3)
			stary(50,  0.5f, 80, 100, 40)
			stary(-100, 0.8f, 30, 70, 5)
		}

		def star(c: Vec, radius1: Float, radius2: Float, n: Int) = {	
			def onCircle(rad: Float, angle: Float, n: Int) = 
				((Vec(rad, angle) -> Vec(0, 1.0f / n)).fromPolar + c).toList(n)

			val outer = onCircle(radius2, 0.5f / n, n)
			val inner = { val xs = onCircle(radius1, 0, n); xs ++ List(xs.head) }

			Contour(interperse(inner, outer)).close			
		}

		def interperse[A](as: List[A], bs: List[A]): List[A] = (as, bs) match {
			case (Nil, b) => b
			case (a, Nil) => a
			case (x::xs, y::ys) => x :: y :: interperse(xs, ys)
		}
	}

	object TriangleStrip {
		val outsideRadius = 150
		val insideRadius  = 100
		val numPoints = mouseX.map(x => map(x, 0, winSize.x, 6, 60).toInt)

		def main = grayWin.run {
			numPoints.map(tris)
		}

		def tris(n: Int) = star(winSize/2, insideRadius, outsideRadius, n)

		def star(c: Vec, radius1: Float, radius2: Float, n: Int) = {	
			def onCircle(rad: Float, angle: Float, n: Int) = 
				((Vec(rad, angle) -> Vec(0, 1.0f / n)).fromPolar + c).toList(n + 1)

			val outer = onCircle(radius2, 0.5f / n, n)
			val inner = onCircle(radius1, 0, n)

			Contour(interperse(inner, outer)).triangleStrip			
		}

		def interperse[A](as: List[A], bs: List[A]): List[A] = (as, bs) match {
			case (Nil, b) => b
			case (a, Nil) => a
			case (x::xs, y::ys) => x :: y :: interperse(xs, ys)
		}
	}

	object Bezier {

		def main = blackWin.setup{noFill; stroke(255)}.run {
			mouseX.map { x => 
				for (i <- 0 until 200 by 20) {
					 bezier(x-(i/2.0f), 40+i, 410, 20, 440, 300, 240-(i/16.0f), 300+(i/8.0f))
				}
			}
		}
	}

	object ThreeD {

		def main = Win(size = winSize, bkg = Color(0), renderer = P3D).run {
			Get.const(display)
		}

		def display {
			lights

			noStroke
			inSpace(Vec3(130, winSize.y, 0), Vec3(-0.1f, 0.4f, 0)) {
				box(100)
			}

			stroke(255)
			inSpace(Vec3(500, winSize.y * 0.35f, -200)) {
				sphere(280)
			}
		}
	}


}