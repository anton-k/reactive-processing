
package rea.examples

import rea._
import P._

object Interaction {

	val winSize = Vec(640, 360)
	val win = Win(size = winSize, bkg = Color(200))
	val blackWin = Win(size = winSize, bkg = Color(0))

	object Tickle {
		val msg = "tickle"

		case class St(font: Font, msgSize: Vec)

		def setup = {
			textAlign(CenterX, CenterY)
			noStroke
			fill(0)
			val font = Font("Georgia", 36)
			St(font, font.textSize(msg) / 2)
		}

		def go(b: Boolean, dx: Vec) = if (b) dx else Vec(0, 0)
		def checkInside(size: Vec)(m: Vec, c: Vec): Boolean = (m - c).inside(Rect(-size, size * 2))

		def draw(st: St) = {
			lazy val pos = (winSize / 2) -> Get.lift(go, isInside, Vec(10, 10).rnd - Vec(5, 5))
			lazy val isInside: Get[Boolean] = Get.lift(checkInside(st.msgSize) _, mouse, pos)
			pos.map(v => text(msg, v))
		}


		def main = win.run(setup, draw)
	}


	def follow(segLength: Float, pos: Vec, target: Get[Vec]): Get[Vec] = {
		def go(p: Vec, t: Vec): Vec = {
			val dp = t - p
			val ang = dp.atan2
			t - Vec.norm(ang) * segLength
		}			

		Get.move(target, pos, go)
	}
 

	object Follow1 {
		val segLength = 50

		def drawSegment(segLength: Float, pos: Vec, target: Vec) {
			inSpace(pos, (target - pos).atan2) { line(0, 0, segLength, 0) }
			circle(pos, 20)
		}

		def main = blackWin.setup{
			strokeWeight(20)
			stroke(255, 100)
		}.run {			
			Get.lift(drawSegment(segLength, (_: Vec), (_: Vec)), follow(segLength, winSize/2, mouse), mouse)
		}

	}

	object Follow2 {
		val segLength = 50

		def drawSegment(segLength: Float, pos: Vec, target: Vec) {
			inSpace(pos, (target - pos).atan2) { line(0, 0, segLength, 0) }			
		}

		def draw(a: Vec, b: Vec, c: Vec) {
			drawSegment(segLength, a, b)
			drawSegment(segLength, b, c)
		}

		def main = blackWin.setup{
			strokeWeight(20)
			stroke(255, 100)
		}.run {	
			val p1 = mouse
			val p2 = follow(segLength, winSize/2, p1)			
			val p3 = follow(segLength, winSize/2, p2)

			Get.lift(draw, p1, p2, p3)
		}
	}

	object Follow3 {
		val segLength = 18
		val segs = (1 to 60).map(x => winSize/2).toList

		def drawHead(pos: Vec, target: Vec) {
			inSpace(pos, (target - pos).atan2) { line(0, 0, segLength, 0) }
		}

		def drawRest(pos: Vec, target: Vec) {
			line(pos, target)
		}		

		def drawChain(target: Vec, chain: List[Vec]) {
			drawHead(target, chain.head)
			chain.sliding(2).foreach { case x::y::Nil => drawRest(x, y)	}
		}

		type St = (Get[Vec], List[Get[Vec]])

		val initSt: St = (mouse, Nil)

		def collect(res: St, pos: Vec): St = {
			val a = follow(segLength, pos, res._1)
			(a, a :: res._2)
		}

		def main = blackWin.setup{
			strokeWeight(10)
			stroke(255, 100)
		}.run {	
			val chain = Get.sequence(segs.foldLeft(initSt)(collect)._2).map(_.reverse)
			Get.lift(drawChain, mouse, chain)
		}
	}

}