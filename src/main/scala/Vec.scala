package rea

object Vec {
	def apply(a: Float): Vec = Vec(a, a)
	def apply(a: Get[Float]): Get[Vec] = a.map(Vec(_))

	def apply(a: Get[Float], b: Get[Float]): Get[Vec] = 
		Get.lift(Vec.apply _, a, b)

	def apply(a: Float, b: Get[Float]): Get[Vec] = 
		b.map(x => Vec(a, x))

	def apply(a: Get[Float], b: Float): Get[Vec] = 
		a.map(x => Vec(x, b))
}

case class Vec(x: Float, y: Float) {
	def +(that: Vec) = Vec(x + that.x, y + that.y)
	def -(that: Vec) = this + that * (-1)
	def *(k: Float)  = Vec(x * k, y * k)
	def *(that: Vec) = Vec(x * that.x, y * that.y)
	def /(k: Float) = Vec(x / k, y / k)
	def mul(that: Vec): Float = x * that.x + y * that.y

	// aux

	def magnitude: Float = Math.sqrt(this.mul(this)).toFloat
	def normalize: Vec = this / this.magnitude
	def dist(that: Vec): Float = (this - that).magnitude

	def vel(v: => Get[Vec]) = 
		Get.fsm(v, this, (a: Vec, b: Vec) => a + b, (x: Vec) => x)

	def vel(v: Vec): Get[Vec] = 
		Get.iter(this, (_: Vec) + v)

	def ->(v: Vec): Get[Vec] = this.vel(v)

	def ->(v: Get[Vec]): Get[Vec] = this.vel(v)

	def acc(v0: Vec, a: Vec) = 
		this.vel(v0.vel(a))

	def acc(v0: Vec, a: => Get[Vec]) = 
		this.vel(v0.vel(a))

	// lifted methods

	def +(that: Get[Vec]): Get[Vec] = that.map(x => this + x)
	def -(that: Get[Vec]): Get[Vec] = that.map(x => this - x)
	def *(that: Get[Vec]): Get[Vec] = that.map(x => this * x)	
	
	def mul(that: Get[Vec]): Get[Float] = that.map(x => this.mul(x)) 
	def dist(that: Get[Vec]): Get[Float] = that.map(x => this.dist(x)) 

	// polar

	def fromPolar = Vec(x * Math.cos(2 * Math.PI * y).toFloat, x * Math.sin(2 * Math.PI * y).toFloat)
	def toPolar = Vec(this.magnitude, (Math.atan2(y, x) / (2 * Math.PI)).toFloat)

	// random

	def rnd: Get[Vec] = Get.lift(Vec.apply _, Get.rnd(x), Get.rnd(y))

	def noise(a: Vec = Vec(0, 1000), dt: Vec = Vec(0.01f, 0.01f)): Get[Vec] = 
		Get.lift(Vec.apply _, Get.noise(a.x, dt.x).map(_ * x), Get.noise(a.y, dt.y).map(_ * y))

	// limiters

	def limit(a: Vec, b: Vec): Vec = {
		def lim(t: Float, a: Float, b: Float) = t.max(a).min(b)

		Vec(lim(x, a.x, b.x), lim(y, a.y, b.y))
	}

	def limit(r: Rect): Vec = limit(Vec(r.x, r.y), Vec(r.x + r.w, r.y + r.h))

	def inside(r: Rect): Boolean = x >= r.x && x <= (r.x + r.w) && y >= r.y && y <= (r.y + r.h)
}

// ----------------------------------------------------------------------------------------------

object Vec3 {
	def apply(a: Float): Vec3 = Vec3(a, a, a)
	def apply(a: Get[Float]): Get[Vec3] = a.map(Vec3(_))
	
	def apply(v: Vec, z: Float): Vec3 = Vec3(v.x, v.y, z)
	def apply(v: Get[Vec], z: Float): Get[Vec3] = v.map(Vec3(_, z))
	def apply(v: Vec, z: Get[Float]): Get[Vec3] = z.map(Vec3(v, _))
	def apply(v: Get[Vec], z: Get[Float]): Get[Vec3] = Get.lift(Vec3((_:Vec), (_:Float)), v, z)

	def apply(x: Get[Float], y: Float, z: Float): Get[Vec3] = x.map(Vec3((_:Float), y, z))
	def apply(x: Float, y: Get[Float], z: Float): Get[Vec3] = y.map(Vec3(x,(_:Float),z))
	def apply(x: Float, y: Float, z: Get[Float]): Get[Vec3] = z.map(Vec3(x,y,(_:Float)))

	def apply(x: Get[Float], y: Get[Float], z: Float): Get[Vec3] = Get.lift(Vec3((_:Float), (_:Float), z), x, y)
	def apply(x: Get[Float], y: Float, z: Get[Float]): Get[Vec3] = Get.lift(Vec3((_:Float), y, (_:Float)), x, z)
	def apply(x: Float, y: Get[Float], z: Get[Float]): Get[Vec3] = Get.lift(Vec3(x, (_:Float), (_:Float)), y, z)
}

case class Vec3(x: Float, y: Float, z: Float) {
	def +(that: Vec3) = Vec3(x + that.x, y + that.y, z + that.z)
	def -(that: Vec3) = this + that * (-1)
	def *(k: Float)  = Vec3(x * k, y * k, z * k)
	def *(that: Vec3) = Vec3(x * that.x, y * that.y, z * that.z)
	def /(k: Float) = this * (1 / k)
	def mul(that: Vec3): Float = x * that.x + y * that.y + z * that.z

	// aux

	def magnitude: Float = Math.sqrt(this.mul(this)).toFloat
	def normalize: Vec3 = this / this.magnitude
	def dist(that: Vec3): Float = (this - that).magnitude

	def vel(v: => Get[Vec3]) = 
		Get.fsm(v, this, (a: Vec3, b: Vec3) => a + b, (x: Vec3) => x)

	def vel(v: Vec3): Get[Vec3] = 
		Get.iter(this, (_: Vec3) + v)

	def ->(v: Vec3): Get[Vec3] = this.vel(v)

	def ->(v: Get[Vec3]): Get[Vec3] = this.vel(v)

	def acc(v0: Vec3, a: Vec3) = 
		this.vel(v0.vel(a))

	def acc(v0: Vec3, a: => Get[Vec3]) = 
		this.vel(v0.vel(a))

	// lifted methods

	def +(that: Get[Vec3]): Get[Vec3] = that.map(x => this + x)
	def -(that: Get[Vec3]): Get[Vec3] = that.map(x => this - x)
	def *(that: Get[Vec3]): Get[Vec3] = that.map(x => this * x)	
	
	def mul(that: Get[Vec3]): Get[Float] = that.map(x => this.mul(x)) 
	def dist(that: Get[Vec3]): Get[Float] = that.map(x => this.dist(x)) 

	// polar

	def fromPolar = Vec3(
		x * Math.sin(2 * Math.PI * y).toFloat * Math.cos(2 * Math.PI * z).toFloat, 
		x * Math.sin(2 * Math.PI * y).toFloat * Math.sin(2 * Math.PI * z).toFloat,
		x * Math.cos(2 * Math.PI * y).toFloat)

	def toPolar = {
		val r = this.magnitude
		Vec3(r, (Math.acos(z / r) / (2 * Math.PI)).toFloat, (Math.atan2(y, x) / (2 * Math.PI)).toFloat)
	}

	// random

	def rnd: Get[Vec3] = Get.lift(Vec3.apply _, Get.rnd(x), Get.rnd(y), Get.rnd(z))

	def noise(a: Vec3 = Vec3(0, 1000, 100000), dt: Vec3 = Vec3(0.01f, 0.01f, 0.01f)): Get[Vec3] = 
		Get.lift(Vec3.apply _, Get.noise(a.x, dt.x).map(_ * x), Get.noise(a.y, dt.y).map(_ * y), Get.noise(a.z, dt.z).map(_ * z))

	// limiters

	def limit(a: Vec3, b: Vec3): Vec3 = {
		def lim(t: Float, a: Float, b: Float) = t.max(a).min(b)

		Vec3(lim(x, a.x, b.x), lim(y, a.y, b.y), lim(z, a.z, b.z))
	}	
}

// ----------------------------------------------------------------------------------------------

object Rect {
	def apply(pos: Vec, sizes: Vec): Rect = Rect(pos.x, pos.y, sizes.x, sizes.y)
}

case class Rect(x: Float, y: Float, w: Float, h: Float) 


// ----------------------------------------------------------------------------------------------


case class Body(m: Float, pos: Vec, vel: Vec, velLimit: Float = 10) {

	def move(force: => Get[Vec]): Get[Body] = {
		val getVel = vel.vel(force.map(x => x / m)).map((v: Vec) => if (v.magnitude > velLimit) (v.normalize * velLimit) else v)
		val getPos = pos.vel(getVel)
		Get.lift((p: Vec, v: Vec) => Body(m, p, v, velLimit), getPos, getVel)
	}

}
