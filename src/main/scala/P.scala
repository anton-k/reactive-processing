import processing.core._

object P {
	

	// an empty project

	private var setups = () => {}
	private var draws  = () => {}

	private val app = new PApplet {
		override def setup = setups()
		override def draw  = draws()
	}

	// ---------------------------------------------------------------------------
	// converters

	def fromTau(a: Float): Float = PConstants.TWO_PI * a	

	// ---------------------------------------------------------------------------
	// processing primitives
	
	// attributes

	def ellipseMode(a: Int) { app.ellipseMode(a) }
	def ellipseModeCenter = ellipseMode(PConstants.CENTER)
	def ellipseModeRadius = ellipseMode(PConstants.RADIUS)
	def ellipseModeCorner = ellipseMode(PConstants.CORNER)
	def ellipseModeCorners = ellipseMode(PConstants.CORNERS)

	def rectMode(a: Int) { app.rectMode(a) }
	def rectModeCenter = rectMode(PConstants.CENTER)
	def rectModeRadius = rectMode(PConstants.RADIUS)
	def rectModeCorner = rectMode(PConstants.CORNER)
	def rectModeCorners = rectMode(PConstants.CORNERS)

	def smooth 		{ app.smooth() }
	def noSmooth 	{ app.noSmooth() }

	// Color settings

	def background(c: Color) 	{ app.background(c.toProc) }	
	def fill(c: Color) 			{ app.noStroke(); app.fill(c.toProc) }	
	def stroke(c: Color) 		{ app.noFill(); app.stroke(c.toProc) }
	def fillStroke(c1: Color, c2: Color) { app.fill(c1.toProc); app.stroke(c2.toProc) }
	def strokeWeight(w: Int)	{ app.strokeWeight(w) }

	// 2D primitives
	
	def ellipse(p: Vec, s: Vec) { app.ellipse(p.x, p.y, s.x, s.y) }	
	def line(p1: Vec, p2: Vec)  { app.line(p1.x, p1.y, p2.x, p2.y) }	
	def line(p1: Vec3, p2: Vec3)  { app.line(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z) }
	def point(p: Vec)			{ app.point(p.x, p.y) }	
	def point(p: Vec3)			{ app.point(p.x, p.y, p.z) }	
	def quad(p1: Vec, p2: Vec, p3: Vec, p4: Vec) { app.quad(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y) }
	def rect(p: Vec, s: Vec)	{ app.rect(p.x, p.y, s.x, s.y) }
	def triangle(p1: Vec, p2: Vec, p3: Vec) { app.triangle(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y) }

	// 3D primitives

	def box(size: Float)		{ app.box(size) }	
	def sphere(r: Float)		{ app.sphere(r) }

	// Transform

	def pushMatrix 				{ app.pushMatrix() }
	def popMatrix				{ app.popMatrix() }
	def translate(v: Vec)		{ app.translate(v.x, v.y) }
	def translate(v: Vec3)		{ app.translate(v.x, v.y, v.z) }
	def rotate(angle: Float)	{ app.rotate(fromTau(angle)) }
	def scale(r: Float)			{ app.scale(r) }
	def scale(v: Vec)			{ app.scale(v.x, v.y) }
	def scale(v: Vec3)			{ app.scale(v.x, v.y, v.z) }

	// ------------------------------------
	// Input

	// Mouse
	def mouse: Get[Vec] 		= Vec(Get.ask(app.mouseX.toFloat), Get.ask(app.mouseY.toFloat))
	def pmouse: Get[Vec]		= Vec(Get.ask(app.pmouseX.toFloat), Get.ask(app.pmouseY.toFloat))	

	def mousePressed: Evt[Vec]  = Evt(app.mousePressed, Vec(app.mouseX.toFloat, app.mouseY.toFloat))

	// Keyboard
	
	def key: Evt[Char]			= Evt(app.keyPressed, app.key)

	// ---------------------------------------------------------------------------
	// Time & date

	def day: Get[Int] 		= Get.ask(PApplet.day())
	def hour: Get[Int] 		= Get.ask(PApplet.hour())
	def millis: Get[Int] 	= Get.ask(app.millis())
	def minute: Get[Int] 	= Get.ask(PApplet.minute())
	def month: Get[Int] 	= Get.ask(PApplet.month())
	def second: Get[Int] 	= Get.ask(PApplet.second())
	def year: Get[Int] 		= Get.ask(PApplet.year())

	// ---------------------------------------------------------------------------
	// Typography

	trait Font {
		var font: PFont

		def init: Unit

		def set {
			app.textFont(font)
		}

		def text(msg: String, pos: Vec) {
			this.set
			app.text(msg, pos.x, pos.y)
		}
	}

	object Font {
		def create(name: String, size: Int = 16, smooth: Boolean = true) = new Font {
			var font: PFont = null

			def init {
				font = app.createFont(name, size, smooth)
				app.textFont(font)
			}			
		}

		def load(file: String) = new Font {
			var font: PFont = null 

			def init {
				font = app.loadFont(file)
				app.textFont(font)
			}			
		}
	}

	def text(msg: String, pos: Vec) {
		app.text(msg, pos.x, pos.y)
	}

	def textWidth(msg: Char): Float = app.textWidth(msg)
	def textWidth(msg: String): Float = app.textWidth(msg)

	trait AlignX {
		def toProc: Int
	}

	case object LeftX 	extends AlignX { def toProc = PConstants.LEFT }
	case object RightX 	extends AlignX { def toProc = PConstants.RIGHT }
	case object CenterX extends AlignX { def toProc = PConstants.CENTER }

	trait AlignY {
		def toProc: Int
	}

	case object TopY 		extends AlignY { def toProc = PConstants.TOP }
	case object BottomY 	extends AlignY { def toProc = PConstants.BOTTOM }
	case object CenterY 	extends AlignY { def toProc = PConstants.CENTER }
	case object BaselineY 	extends AlignY { def toProc = PConstants.BASELINE }

	def textAlign(x: AlignX, y: AlignY = BaselineY) { app.textAlign(x.toProc, y.toProc)	}
	def textLeading(a: Int) { app.textLeading(a) }
	def textSize(a: Int) { app.textSize(a) }
	def textModeShape = { app.textMode(PConstants.SHAPE) }
	def textModeModel = { app.textMode(PConstants.MODEL) }

	// ---------------------------------------------------------------------------
	// files

	def loadStrings(file: String): List[String] = app.loadStrings(file).toList
	def saveStrings(file: String, data: Seq[String]) { app.saveStrings(file, data.toArray) }

	// ---------------------------------------------------------------------------

	// 2D primitives

	def circle(p: Vec, rad: Float) { ellipseModeCenter; ellipse(p, Vec(rad, rad)) }
	
	// Transform

	def inSpace(v: Vec, angle: Float)(act: => Unit) = withMatrix {
		translate(v)
		rotate(angle)
		act		
	}

	def withMatrix(act: => Unit) {
		pushMatrix
			act
		popMatrix 
	}

	def onCircle(center: Vec, angle: Float, rad: Float, speed: Float): Get[Vec] = 
		(Vec(rad, angle) -> Vec(0, speed)).fromPolar + center


	// ---------------------------------------------------------------------------
	//

	trait Renderer
	case object DefaultRenderer extends Renderer
	case object P2D extends Renderer
	case object P3D extends Renderer
	case object OpenGL extends Renderer

	case class Win(
		name: String = "App", 
		sizes: Vec = Vec(500, 500), 
		bkg: Color = Color.white, 
		clear: Boolean = true, 
		renderer: Renderer = DefaultRenderer,
		fonts: Seq[Font] = Seq()) {

		def run(getter: Get[Unit]) {
			def newSetups() { 
				background(bkg)
				renderer match {
					case DefaultRenderer => app.size(sizes.x.toInt, sizes.y.toInt)
					case P2D => app.size(sizes.x.toInt, sizes.y.toInt, PConstants.P2D)
					case P3D => app.size(sizes.x.toInt, sizes.y.toInt, PConstants.P3D)
					case OpenGL => app.size(sizes.x.toInt, sizes.y.toInt, PConstants.OPENGL)
				}
				fonts.foreach(_.init)
			}				 

			def newDraws()  { 
				if (clear) { background(bkg) }
				if (getter.isAlive) {
					getter.get
					getter.step
				}
			}

			setups = newSetups
			draws  = newDraws

			try {
				PApplet.runSketch(Array(name), app)
			} catch {
				case e: Throwable => app.dispose()
			}
		}
	}

	// -----------------------------------------------------------------------------------
	// colors

	case class Color(red: Float, green: Float, blue: Float, alpha: Float = 1) {
		def toProc = app.color(255 * red, 255 * green, 255 * blue, 255 * alpha)
	}

	object Color {
		def apply(a: Float): Color = Color(a, a, a)
		def apply(a: Float, b: Float): Color = Color(a, a, a, b)

		def rnd(): Get[Color] = 
			Get.lift((r: Float, g: Float, b: Float) => Color(r, g, b), Get.rnd(), Get.rnd(), Get.rnd())

		def rndAlpha(): Get[Color] = 
			Get.lift((r: Float, g: Float, b: Float, a: Float) => Color(r, g, b, a), Get.rnd(), Get.rnd(), Get.rnd(), Get.rnd())

		def black() = Color(0)

		def white() = Color(1)
	}

	object Brush {
		def fill(c: Color) = Brush(Some(c), None, None)
		def stroke(c: Color) = Brush(None, Some(c), None)
		def strokeWeight(c: Color, w: Int) = Brush(None, Some(c), Some(w))
		def apply(fill: Color, stroke: Color): Brush = Brush(Some(fill), Some(stroke), None)
	}

	case class Brush(fill: Option[Color] = None, stroke: Option[Color] = None, weight: Option[Int] = None) {
		def toProc {
			fill.map(col => app.fill(col.toProc)).getOrElse(app.noFill())
			stroke.map(col => app.stroke(col.toProc)).getOrElse(app.noStroke)
			weight.foreach(app.strokeWeight(_))
		}

		def onShape(shape: PShape) {
			fill.map(col => shape.fill(col.toProc)).getOrElse(shape.noFill())
			stroke.map(col => shape.stroke(col.toProc)).getOrElse(shape.noStroke)
			weight.foreach(shape.strokeWeight(_))
		}
	}


	// -----------------------------------------------------------------------------------
	// PShape

	def contours(cs: Seq[Contour]) {
		app.beginShape()
			cs.foreach(c => c.draw())
		app.endShape()
	}

	case class Contour(ps: Seq[Vec], b: Brush = Brush()) {
		def render(shape: PShape) {
			b.onShape(shape)
			shape.beginContour()
				ps.foreach(p => shape.vertex(p.x, p.y))			
			shape.endContour()
		}

		def draw() {
			b.toProc
			app.beginContour()
				ps.foreach(p => app.vertex(p.x, p.y))			
			app.endContour()
		}
	}

	case class Shape(var shape: PShape) {
		def draw() {
			app.shape(shape)
		}

		def setBrush(b: Brush) {
			b.onShape(shape)
		}

		def translate(v: Vec) {
			shape.translate(v.x, v.y)
		}

		def translate(v: Vec3) {
			shape.translate(v.x, v.y, v.z)
		}

		def rotate(a: Float) {
			shape.rotate(fromTau(a))
		}

		def rotate(v: Vec3) {
			shape.rotateX(v.x)
			shape.rotateY(v.y)
			shape.rotateZ(v.z)
		}

		def rotateX(a: Float) {
			shape.rotateX(fromTau(a))
		}

		def rotateY(a: Float) {
			shape.rotateY(fromTau(a))
		}

		def rotateZ(a: Float) {
			shape.rotateZ(fromTau(a))
		}

		def scale(r: Float) {
			shape.scale(r)
		}

		def scale(r: Vec) {
			shape.scale(r.x, r.y)
		}

		def scale(r: Vec3) {
			shape.scale(r.x, r.y, r.z)
		}
	}

	object Shape {
		private def vertices(shape: PShape, ps: Seq[Vec]) {
			ps.foreach(p => shape.vertex(p.x, p.y))
		}	

		def group(as: Seq[Shape]) = Shape {
			var shape = app.createShape(PConstants.GROUP)
			as.foreach(x => shape.addChild(x.shape))
			shape
		}

		def ellipse(p: Vec, s: Vec) = Shape  {
			app.createShape(PConstants.ELLIPSE, p.x, p.y, s.x, s.y)
		}

		def rect(p: Vec, s: Vec) = Shape {
			app.createShape(PConstants.RECT, p.x, p.y, s.x, s.y)
		}

		def line(p1: Vec, p2: Vec) = Shape {
			app.createShape(PConstants.LINE, p1.x, p1.y, p2.x, p2.y)
		}

		def line(p1: Vec3, p2: Vec3) = Shape {
			app.createShape(PConstants.LINE, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z)
		}

		def triangle(p1: Vec, p2: Vec, p3: Vec) = Shape {
			app.createShape(PConstants.TRIANGLE, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y)
		}		

		def quad(p1: Vec, p2: Vec, p3: Vec, p4: Vec) = Shape {
			app.createShape(PConstants.QUAD, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y)
		}

		def sphere(r: Float) = Shape {
			app.createShape(PConstants.SPHERE, r)
		}		

		def box(s: Float) = Shape {
			app.createShape(PConstants.BOX, s)
		}

		def box(s: Vec3) = Shape {
			app.createShape(PConstants.BOX, s.x, s.y, s.z)
		}

		def close(ps: Seq[Vec]) = Shape {
			var shape = app.createShape()
			shape.beginShape()
				vertices(shape, ps)
			shape.endShape(PConstants.CLOSE)
			shape
		}

		def open(ps: Seq[Vec]) = Shape {
			var shape = app.createShape()
			shape.beginShape()
				vertices(shape, ps)
			shape.endShape()
			shape
		}

		def shapeForType(shapeType: Int, ps: Seq[Vec], b: Brush = Brush()) = Shape {
			var shape = app.createShape()
			shape.beginShape(shapeType)
				b.onShape(shape)
				vertices(shape, ps)
			shape.endShape()
			shape
		}

		def points(ps: Seq[Vec], b: Brush = Brush()) = shapeForType(PConstants.POINTS, ps, b)
		def lines(ps: Seq[Vec], b: Brush = Brush()) = shapeForType(PConstants.LINES, ps, b)
		def triangles(ps: Seq[Vec], b: Brush = Brush()) = shapeForType(PConstants.TRIANGLES, ps, b)
		def triangleFan(ps: Seq[Vec], b: Brush = Brush()) = shapeForType(PConstants.TRIANGLE_FAN, ps, b)
		def triangleStrip(ps: Seq[Vec], b: Brush = Brush()) = shapeForType(PConstants.TRIANGLE_STRIP, ps, b)
		def quads(ps: Seq[Vec], b: Brush = Brush()) = shapeForType(PConstants.QUADS, ps, b)
		def quadStrip(ps: Seq[Vec], b: Brush = Brush()) = shapeForType(PConstants.QUAD_STRIP, ps, b)		

		def contours(cs: Seq[Contour]) = Shape {
			var shape = app.createShape()
			shape.beginShape()
				cs.foreach(c => c.render(shape))
			shape.endShape()
			shape
		}		
	}

	// -----------------------------------------------------------------------------------
	// math funs

	def lerp(a: Float, b: Float, k: Float) = a + (b - a) * k
	def lerp(a: Vec, b: Vec, k: Float) = a + (b - a) * k
	def lerp(a: Vec3, b: Vec3, k: Float) = a + (b - a) * k	

	// -----------------------------------------------------------------------------------
	// helpers for lifting operations

	implicit class Vecs(vs: Get[Vec]) {
		def fromPolar: Get[Vec] = vs.map(_.fromPolar)
		def toPolar: Get[Vec] = vs.map(_.toPolar)
		def normalize: Get[Vec] = vs.map(_.normalize)
		def magnitude: Get[Float] = vs.map(_.magnitude)

		def +(that: Vec): Get[Vec] = vs.map(_ + that)
		def +(that: Get[Vec]):Get[Vec] = Get.lift((_:Vec) + (_: Vec), vs, that)
		def -(that: Vec): Get[Vec] = vs.map(_ - that)
		def -(that: Get[Vec]):Get[Vec] = Get.lift((_:Vec) - (_: Vec), vs, that)
		def *(that: Float): Get[Vec] = vs.map(_ * that)
		def *(that: Vec): Get[Vec] = vs.map(_ * that)
		def *(that: Get[Vec]):Get[Vec] = Get.lift((_:Vec) * (_: Vec), vs, that)
		def /(that: Float): Get[Vec] = vs.map(_ / that)		
		def /(that: Get[Float]):Get[Vec] = Get.lift((_:Vec) / (_: Float), vs, that)

		def dist(that: Vec): Get[Float] = vs.map(_.dist(that))
		def dist(that: Get[Vec]):Get[Float] = Get.lift((_:Vec).dist(_: Vec), vs, that)
		def mul(that: Vec): Get[Float] = vs.map(_.dist(that))
		def mul(that: Get[Vec]):Get[Float] = Get.lift((_:Vec).mul(_: Vec), vs, that)
	}

	implicit class Vec3s(vs: Get[Vec3]) {
		def fromPolar: Get[Vec3] = vs.map(_.fromPolar)
		def toPolar: Get[Vec3] = vs.map(_.toPolar)
		def normalize: Get[Vec3] = vs.map(_.normalize)
		def magnitude: Get[Float] = vs.map(_.magnitude)

		def +(that: Vec3): Get[Vec3] = vs.map(_ + that)
		def +(that: Get[Vec3]):Get[Vec3] = Get.lift((_:Vec3) + (_: Vec3), vs, that)
		def -(that: Vec3): Get[Vec3] = vs.map(_ - that)
		def -(that: Get[Vec3]):Get[Vec3] = Get.lift((_:Vec3) - (_: Vec3), vs, that)
		def *(that: Float): Get[Vec3] = vs.map(_ * that)
		def *(that: Vec3): Get[Vec3] = vs.map(_ * that)
		def *(that: Get[Vec3]):Get[Vec3] = Get.lift((_:Vec3) * (_: Vec3), vs, that)
		def /(that: Float): Get[Vec3] = vs.map(_ / that)		
		def /(that: Get[Float]):Get[Vec3] = Get.lift((_:Vec3) / (_: Float), vs, that)

		def dist(that: Vec3): Get[Float] = vs.map(_.dist(that))
		def dist(that: Get[Vec3]):Get[Float] = Get.lift((_:Vec3).dist(_: Vec3), vs, that)
		def mul(that: Vec3): Get[Float] = vs.map(_.dist(that))
		def mul(that: Get[Vec3]):Get[Float] = Get.lift((_:Vec3).mul(_: Vec3), vs, that)
	}

}
