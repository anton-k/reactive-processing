package rea

import processing.core._

object P {
	

	// an empty project

	private var setups = () => {}
	private var draws  = () => {}
	private var customSetup = () => {}

	private val app = new PApplet {
		override def setup = setups()
		override def draw  = draws()
	}

	private def runApp(name: String, newSetups: () => Unit, newDraws: () => Unit) {
		setups = newSetups
		draws  = newDraws

		try {
			PApplet.runSketch(Array(name), app)
		} catch {
			case e: Throwable => app.dispose()
		}
	}

	// ---------------------------------------------------------------------------
	// converters

	def fromTau(a: Float): Float = PConstants.TWO_PI * a	
	def toTau(a: Float): Float = a / PConstants.TWO_PI

	// ---------------------------------------------------------------------------
	// processing primitives
	
	// structure

	def noLoop = { app.noLoop() }
	def loop   = { app.loop() }

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
	def fill(c: Color) 			{ app.fill(c.toProc) }	
	def stroke(c: Color) 		{ app.stroke(c.toProc) }
	def noStroke				{ app.noStroke() }
	def noFill					{ app.noFill() }
	def fillStroke(c1: Color, c2: Color) { app.fill(c1.toProc); app.stroke(c2.toProc) }
	def strokeWeight(w: Int)	{ app.strokeWeight(w) }

	// original

	def fill(r: Float, g: Float, b:Float) { app.fill(r, g, b) }	
	def stroke(r: Float, g: Float, b:Float) { app.stroke(r, g, b) }	 

	def fill(r: Float) { app.fill(r) }	
	def stroke(r: Float) { app.stroke(r) }	
	def stroke(r: Float, a: Float) { app.stroke(r, a) }	
	def strokeCapRound = { app.strokeCap(PConstants.ROUND) }
	def strokeCapSquare = { app.strokeCap(PConstants.SQUARE) }
	def strokeCapProject = { app.strokeCap(PConstants.PROJECT) }
	def strokeJoinMiter = { app.strokeJoin(PConstants.MITER) }
	def strokeJoinBevel = { app.strokeJoin(PConstants.BEVEL) }
	def strokeJoinRound = { app.strokeJoin(PConstants.ROUND) } 

	// 2D primitives
	
	def ellipse(p: Vec, s: Vec) { app.ellipse(p.x, p.y, s.x, s.y) }	
	def arc(p: Vec, s: Vec, start: Float, stop: Float) { arc(p.x, p.y, s.x, s.y, start, stop) }
	def line(p1: Vec, p2: Vec)  { app.line(p1.x, p1.y, p2.x, p2.y) }	
	def line(p1: Vec3, p2: Vec3)  { app.line(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z) }
	def point(p: Vec)			{ app.point(p.x, p.y) }	
	def point(p: Vec3)			{ app.point(p.x, p.y, p.z) }	
	def quad(p1: Vec, p2: Vec, p3: Vec, p4: Vec) { app.quad(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y) }
	def rect(p: Vec, s: Vec)	{ app.rect(p.x, p.y, s.x, s.y) }
	def triangle(p1: Vec, p2: Vec, p3: Vec) { app.triangle(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y) }

	def point(x: Float, y: Float) { app.point(x, y) }
	def point(x: Float, y: Float, z: Float) { app.point(x, y, z) }
	def rect(x: Float, y: Float, w: Float, h: Float) { app.rect(x, y, w, h) }
	def ellipse(x: Float, y: Float, w: Float, h: Float) { app.ellipse(x, y, w, h) }
	def arc(x: Float, y: Float, w: Float, h: Float, start: Float, stop: Float) { app.arc(x, y, w, h, fromTau(start), fromTau(stop)) }

	def circle(x: Float, y: Float, rad: Float) { ellipseModeCenter; app.ellipse(x, y, rad*2, rad*2) }
	def line(x1: Float, y1: Float, x2: Float, y2: Float) { app.line(x1, y1, x2, y2) }
	def triangle(x1: Float, y1: Float, x2: Float, y2: Float, x3: Float, y3: Float) { app.triangle(x1, y1, x2, y2, x3, y3) }
	def quad(x1: Float, y1: Float, x2: Float, y2: Float, x3: Float, y3: Float, x4: Float, y4: Float) { app.quad(x1, y1, x2, y2, x3, y3, x4, y4) }
	
	// 3D primitives

	def box(size: Float)		{ app.box(size) }	
	def sphere(r: Float)		{ app.sphere(r) }

	// Vertex

	def beginShape = { app.beginShape() }
	def endShape   = { app.endShape() }
	def endShapeClose = { app.endShape(PConstants.CLOSE) }

	def vertex(x: Float, y: Float) = { app.vertex(x, y) }
	def vertex(x: Float, y: Float, z: Float) = { app.vertex(x, y, z) }
	def vertex(p: Vec): Unit = vertex(p.x, p.y)
	def vertex(p: Vec3): Unit = vertex(p.x, p.y, p.z)

	// Curve

	def bezier(x1: Float, y1: Float, x2: Float, y2: Float, x3: Float, y3: Float, x4: Float, y4: Float) { app.bezier(x1, y1, x2, y2, x3, y3, x4, y4) }
	def bezier(x1: Float, y1: Float, z1: Float, x2: Float, y2: Float, z2: Float, x3: Float, y3: Float, z3: Float, x4: Float, y4: Float, z4: Float) { app.bezier(x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4) }

	def bezier(p1: Vec, p2: Vec, p3: Vec, p4: Vec) { app.bezier(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y) }
	def bezier(p1: Vec3, p2: Vec3, p3: Vec3, p4: Vec3) { app.bezier(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, p3.x, p3.y, p3.z, p4.x, p4.y, p4.z) }

	// Transform

	def pushMatrix 				{ app.pushMatrix() }
	def popMatrix				{ app.popMatrix() }
	def translate(v: Vec)		{ app.translate(v.x, v.y) }
	def translate(v: Vec3)		{ app.translate(v.x, v.y, v.z) }
	def rotate(angle: Float)	{ app.rotate(fromTau(angle)) }
	def rotateX(angle: Float)	{ app.rotateX(fromTau(angle)) }
	def rotateY(angle: Float)	{ app.rotateY(fromTau(angle)) }
	def rotateZ(angle: Float)	{ app.rotateZ(fromTau(angle)) }
	def rotate(v: Vec3)			{ rotateX(v.x); rotateY(v.y); rotateZ(v.z) }
	def rotate(a: Float, v: Vec3) { app.rotate(fromTau(a), v.x, v.y, v.z) }
	def scale(r: Float)			{ app.scale(r) }
	def scale(v: Vec)			{ app.scale(v.x, v.y) }
	def scale(v: Vec3)			{ app.scale(v.x, v.y, v.z) }

	// ------------------------------------
	// Input

	// Mouse
	def mouse: Get[Vec] 		= Vec(mouseX, mouseY)	
	def pmouse: Get[Vec]		= Vec(pmouseX, pmouseY)	

	def mouseY: Get[Float]		= Get.ask(app.mouseY.toFloat)
	def mouseX: Get[Float]		= Get.ask(app.mouseX.toFloat)
	def pmouseY: Get[Float]		= Get.ask(app.pmouseY.toFloat)
	def pmouseX: Get[Float]		= Get.ask(app.pmouseX.toFloat)


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
	// Lights and camera

	def lights = { app.lights() }

	// ---------------------------------------------------------------------------
	// Images

	trait ImageMode {
		def toProc: Int
	}

	case object Rgb   extends ImageMode { def toProc = PConstants.RGB	}
	case object Argb  extends ImageMode { def toProc = PConstants.ARGB	}
	case object Alpha extends ImageMode { def toProc = PConstants.ALPHA	}

	object Image {
		def apply(file: String): Image = Image(app.loadImage(file))
		def apply(w: Int, h: Int, mode: ImageMode): Image = Image(app.createImage(w, h, mode.toProc))
	}

	case class Image(image: PImage) {
		def display { app.image(image, 0, 0) }
		def display(x: Float, y: Float) { app.image(image, x, y) }
		def display(x: Float, y: Float, w: Float, h: Float) { app.image(image, x, y, w, h) }

		def display(v: Vec) { display(v.x, v.y) }
		def display(v: Vec, s: Vec) { display(v.x, v.y, s.x, s.y) }

		def width  = image.width
		def height = image.height
		def pixels = image.pixels	

		def get(x: Int, y: Int) = Color.fromProc(pixels(y*width+x))
	}

	def tint(a: Float) { app.tint(a) }
	def tint(a: Float, alp: Float) { app.tint(a, alp) }
	def tint(r: Float, g: Float, b: Float) { app.tint(r, g, b) }
	def tint(r: Float, g: Float, b: Float, a: Float) { app.tint(r, g, b, a) }
	def tint(c: Color) { tint(c.red, c.green, c.blue, c.alpha) }

	// ---------------------------------------------------------------------------
	// Typography


	object Font {

		private def createAndSet(font: PFont) = {
			app.textFont(font)
			font
		}

		def apply(file: String): Font = Font(createAndSet(app.loadFont(file)))

		def apply(name: String, size: Int, smooth: Boolean = true): Font = 
			Font(createAndSet(app.createFont(name, size, smooth)))
		

	}

	case class Font(font: PFont) {
		def set {
			app.textFont(font)
		}

		def text(msg: String, pos: Vec) {
			this.set
			app.text(msg, pos.x, pos.y)
		}

		def textSize(msg: String): Vec = {
			this.set
			Vec(textWidth(msg), textAscent + textDescent)
		}
	}

	def textAscent = { app.textAscent() }
	def textDescent = { app.textDescent() }

	def text(msg: String, x: Float, y: Float) {
		app.text(msg, x, y)
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

	def inSpace(v: Vec)(act: => Unit) = withMatrix {
		translate(v)		
		act		
	}

	def inSpace(v: Vec3)(act: => Unit) = withMatrix {
		translate(v)		
		act		
	}

	def inSpace(v: Vec3, angle: Vec3)(act: => Unit) = withMatrix {
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

	trait Renderer {
		def toProc: String
	}
	case object DefaultRenderer extends Renderer { def toProc = "" }
	case object P2D 			extends Renderer { def toProc = PConstants.P2D }
	case object P3D 			extends Renderer { def toProc = PConstants.P3D }
	case object OpenGL 			extends Renderer { def toProc = PConstants.OPENGL }
	case object PDF				extends Renderer { def toProc = PConstants.PDF }


	object Pde {
		def apply(name: String, setup: => Unit, draw: => Unit) {
			runApp(name, () => setup, () => draw)
		}
	}

	case class Win(
		name: String = "App", 
		size: Vec = Vec(500, 500), 
		bkg: Color = Color.white, 
		clear: Boolean = true, 
		renderer: Renderer = DefaultRenderer,		
		frameRate: Int = 60) {
		
		private def winSize() = renderer match {
			case DefaultRenderer => app.size(size.x.toInt, size.y.toInt)
			case x               => app.size(size.x.toInt, size.y.toInt, x.toProc)
		}

		private def winInits() {
			background(bkg)
			winSize()
			if (frameRate != 60) {
				app.frameRate(frameRate)
			}	
			customSetup()
		}

		private def runGetter(getter: Get[Unit])() {
			if (clear) { background(bkg) }
			if (getter.isAlive) {
				getter.get
				getter.step
			}			
		}

		def setup(act: => Unit): Win = {
			def mySetup() {	act }
			customSetup = mySetup
			this
		}

		def run {						
			runApp(name, winInits, () => {})
		}

		def run(getter: => Get[Unit]) {
			runApp(name, winInits, runGetter(getter))
		}

		def run[A](setup: => A, draw: A => Get[Unit]) {
			var getter: Get[Unit] = Get.const({})			

			def mySetup() {
				winInits()
				getter = draw(setup)
			}

			def myDraw() {
				if (clear) { background(bkg) }
				if (getter.isAlive) {
					getter.get
					getter.step
				}			
			}

			runApp(name, mySetup, myDraw)
		}

		def pde(setup: => Unit, draw: => Unit) {
			runApp(name, () => { winInits(); setup }, () => draw)
		}
	}

	// -----------------------------------------------------------------------------------
	// colors

	case class Color(red: Float, green: Float, blue: Float, alpha: Float = 255) {
		def toProc = app.color(red, green, blue, alpha)
	}

	object Color {
		def apply(a: Float): Color = Color(a, a, a)
		def apply(a: Float, b: Float): Color = Color(a, a, a, b)

		def fromProc(a: Int): Color = Color(app.red(a).toFloat, app.green(a).toFloat, app.blue(a).toFloat, app.alpha(a).toFloat)

		def rnd(): Get[Color] = 
			Get.lift((r: Float, g: Float, b: Float) => Color(r, g, b), Get.rnd(255), Get.rnd(255), Get.rnd(255))

		def rndAlpha(): Get[Color] = 
			Get.lift((r: Float, g: Float, b: Float, a: Float) => Color(r, g, b, a), Get.rnd(255), Get.rnd(255), Get.rnd(255), Get.rnd(255))

		def black() = Color(0)

		def white() = Color(255)
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

		def close = {
			beginShape
				ps.foreach(vertex)
			endShapeClose
		}

		def triangleStrip = {
			app.beginShape(PConstants.TRIANGLE_STRIP)
				ps.foreach(vertex)
			endShape
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

	def map(x: Float, a: Float, b: Float, c: Float, d: Float) = c + ((x - a) / (b - a)) * (d - c)

	def abs(x: Float): Float = Math.abs(x).toFloat

	// trigonometry

	def cos(x: Float): Float = Math.cos(fromTau(x)).toFloat
	def sin(x: Float): Float = Math.sin(fromTau(x)).toFloat
	def tan(x: Float): Float = Math.tan(fromTau(x)).toFloat

	def acos(x: Float): Float = toTau(Math.acos(x).toFloat)
	def asin(x: Float): Float = toTau(Math.asin(x).toFloat)
	def atan(x: Float): Float = toTau(Math.atan(x).toFloat)

	def atan2(y: Float, x: Float): Float = toTau(Math.atan2(y, x).toFloat)


	// -----------------------------------------------------------------------------------
	// helpers for lifting operations


	implicit class Floats(fs: Get[Float]) {
		def +(that: Float): Get[Float] = fs.map(_ + that)
		def +(that: Get[Float]): Get[Float] = Get.lift((_:Float) + (_:Float), fs, that)
		def -(that: Float): Get[Float] = fs.map(_ - that)
		def -(that: Get[Float]): Get[Float] = Get.lift((_:Float) - (_:Float), fs, that)
		def *(that: Float): Get[Float] = fs.map(_ * that)
		def *(that: Get[Float]): Get[Float] = Get.lift((_:Float) * (_:Float), fs, that)
		def /(that: Float): Get[Float] = fs.map(_ / that)
		def /(that: Get[Float]): Get[Float] = Get.lift((_:Float) / (_:Float), fs, that)
	}

	implicit class Vecs(vs: Get[Vec]) {
		def fromPolar: Get[Vec] = vs.map(_.fromPolar)
		def toPolar: Get[Vec] = vs.map(_.toPolar)
		def normalize: Get[Vec] = vs.map(_.normalize)
		def magnitude: Get[Float] = vs.map(_.magnitude)
		def atan2: Get[Float] = vs.map(_.atan2)
		def l1: Get[Float] = vs.map(_.l1)

		def +(that: Vec): Get[Vec] = vs.map(_ + that)
		def +(that: Get[Vec]):Get[Vec] = Get.lift((_:Vec) + (_: Vec), vs, that)
		def -(that: Vec): Get[Vec] = vs.map(_ - that)
		def unary_- : Get[Vec] = vs.map(x => -x)
		def -(that: Get[Vec]):Get[Vec] = Get.lift((_:Vec) - (_: Vec), vs, that)
		def *(that: Float): Get[Vec] = vs.map(_ * that)
		def *(that: Vec): Get[Vec] = vs.map(_ * that)
		def *(that: Get[Vec]):Get[Vec] = Get.lift((_:Vec) * (_: Vec), vs, that)
		def /(that: Float): Get[Vec] = vs.map(_ / that)		
		def /(that: Get[Float]):Get[Vec] = Get.lift((_:Vec) / (_: Float), vs, that)

		def dist(that: Vec): Get[Float] = vs.map(_.dist(that))
		def l1(that: Vec): Get[Float] = vs.map(_.l1(that))
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
