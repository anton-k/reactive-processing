package rea

import scala.collection.mutable.{ListBuffer, Buffer}
import toxi.math.noise.PerlinNoise
import java.util.Random
import java.io.{BufferedReader, FileReader}

trait Being {
	def isAlive: Boolean	
}

trait Step {
	def step: Unit	

	// A hack to prevent recursive updates.
	// We can update only depends that are 
	// not in the same tick
	var tick: Boolean 

	def nextTick() {
		tick = !tick
	}

	def stepOnTick(deps: Step*)(act: => Unit) {
		nextTick()
		act

		deps.foreach { that => 
			if (tick != that.tick) {
				that.step				
			}
		}
	}
}

trait Getter[+A] { 
	self =>

	def get: A

	def map[B](f: A => B) = new Getter[B] {
		def get = f(self.get)
	}
}

trait Setter[-A] { 
	self =>

	def set(a: A): Unit

	def cmap[B](f: B => A) = new Setter[B] {
		def set(b: B) = self.set(f(b))
	}
}

trait Get[+A] extends Getter[A] with Step with Being { self => 
	override def map[B](f: A => B) = new Get[B] {
		def get = f(self.get)		
		def isAlive = self.isAlive
		var tick = self.tick

		def step = stepOnTick(self) {}
	}

	def take(n: Int): Get[A] = new Get[A] {
		var st = n

		def isAlive = self.isAlive && st > 0		
		def get  = self.get
		def step = stepOnTick(self) { st -= 1 }
		var tick = self.tick
	}

	def fsm[S,B](s0: S, f: (S,A) => S, g: S => B): Get[B] = new Get[B] {
		var s = s0
		var a = g(s)

		def isAlive = self.isAlive		
		def get     = a

		def step    = stepOnTick(self) { s = f(s, self.get); a = g(s) }
		var tick    = self.tick
	}

	def toList(n: Int): List[A] = if (n > 0 && isAlive) {
		get :: { step; this.toList(n - 1) }
	} else { 
		Nil 
	}
	
	def toList(): List[A] = if (isAlive) {
		get :: { step; this.toList() }
	} else { 
		Nil 
	}	

	def toStream(): Stream[A] = 
		get #:: { step; this.toStream() }

	def toIterator: Iterator[A] = 
		this.toStream.toIterator

	def snap(pred: => Boolean): Evt[A] = Evt {
		def phi(p: Boolean, a: A) = if (p) Some(a) else None 

		Get.lift(phi, Get.ask(pred), this)
	}
}

trait Set[-A] extends Setter[A] with Step with Being {
	self =>

	override def cmap[B](f: B => A) = new Set[B] {
		def set(b: B) = self.set(f(b))
		def step = stepOnTick(self) {}
		def isAlive = self.isAlive
		var tick = self.tick
	}
}


trait Alive extends Being {
	def isAlive = true
}

trait NoStep extends Step {
	def step = stepOnTick(){}
	var tick = true	
}


object Get {

	def fsm[S,A,B](dtape: => Get[A], s0: S, f: (S,A) => S, g: S => B): Get[B] = {
		lazy val tape = dtape

		new Get[B] {		
			var s = s0
			var a = g(s)
			var isFirstAlive = true
			
			def isAlive = isFirstAlive

			def step    = stepOnTick(tape) { 
				isFirstAlive = tape.isAlive
				s = f(s, tape.get)
				a = g(s)			
			}
			var tick = true

			def get     = a
		}
	}


	def move[A,B](force: => Get[A], s0: B, append: (B, A) => B): Get[B] = 
		fsm(force, s0, append, (x: B) => x)

	def iter[A](a0: A, upd: A => A): Get[A] = new Get[A] with Alive {
		var a = a0		

		def get = a

		def step = {
			nextTick()
			a = upd(a)
		}
		var tick = true
	}

	def gen[S, A](s0: S, query: S => A): Get[A] = iter(query(s0), (s: A) => query(s0))

	def ask[A](mkAsk: => A): Get[A] = new Get[A] with Alive with NoStep {
		def get = mkAsk
	}

	def const[A](a: => A): Get[A] = new Get[A] with Alive with NoStep {
		def get = a		
	}

	def count(): Get[Int] = iter(0, (x: Int) => x + 1)

	def count(a: Int): Get[Int] = iter(a, (x: Int) => x + 1)

	def count(a: Int, b: Int): Get[Int] = count(a).take(b - a)

	def lin(x: Float, dx: Float) = iter(x, (a: Float) => a + dx)

	def saw(s0: Float, ds: Float, min: Float, max: Float) = Get.iter(s0, (s: Float) => 
		if (s > max) min 
		else if (s < min) max
		else s + ds
	)

	def rnd(): Get[Float] = gen(new Random(), (x: Random) => x.nextFloat())

	def rnd(a: Float): Get[Float] = rnd().map(_ * a)

	def rnd(a: Float, b: Float): Get[Float] = rnd().map((x: Float) => a + x * (b - a))

	def rndInt(a: Int): Get[Int] = gen(new Random(), (x: Random) => x.nextInt(a))

	def rndInt(a: Int, b: Int): Get[Int] = rndInt(b - a).map(x => x + a)

	def rndBoolean() = gen(new Random, (x: Random) => x.nextBoolean)

	def normalRnd(): Get[Float] = gen(new Random(), (x: Random) => x.nextGaussian.toFloat)

	def normalRnd(mean: Float, dist: Float): Get[Float] = normalRnd().map(x => x * dist + mean) 

	def noise(x: Float, dx: Float): Get[Float] = {
		val gen = new PerlinNoise()		
		lin(x, dx).map(a => gen.noise(a))
	}

	// (Applicative functor)

	def sequence[A](as: List[Get[A]]): Get[List[A]] = as match {
		case Nil   => Get.const(Nil)
		case x::xs => lift((_:A) :: (_:List[A]), x, sequence(xs))
	}

	def sequenceActs(as: List[Get[Unit]]): Get[Unit] = new Get[Unit] {
		var tick = true

		def isAlive = as.exists(_.isAlive)

		def step = stepOnTick(as: _*) {
			as.foreach(_.step)
		}

		def get = {
			as.foreach(_.get)
		}
	}

	// File input

	def reader(file: String) = new Get[String] {
		var buf = new BufferedReader(new FileReader(file))
		var canRead = true
		var str: Option[String] = None
		var tick = true

		def step = stepOnTick() {
			if (isAlive) {
				str = Some(buf.readLine())
			} else {
				buf.close()
			}
		}

		def get = {
			str match {
				case None => { step; get }
				case Some(a) => a
			}
		}

		def isAlive = (buf != null) && (str != Some(null))
	}

	
	// Lifters (Applicative functor)

	def lift[A1,A2,B](f: (A1,A2)=>B, da1: => Get[A1], da2: => Get[A2]): Get[B] = {
		lazy val a1 = da1
		lazy val a2 = da2

		new Get[B] {
			def isAlive = a1.isAlive && a2.isAlive
			def step    = stepOnTick(a1, a2) {}
			def get     = f(a1.get, a2.get)
			var tick    = a1.tick
		}
	}

	def lift[A1,A2,A3,B](f: (A1,A2,A3)=>B, da1: => Get[A1], da2: => Get[A2], da3: => Get[A3]): Get[B] = {
		lazy val a1 = da1
		lazy val a2 = da2		
		lazy val a3 = da3

		new Get[B] {
			def isAlive = a1.isAlive && a2.isAlive && a3.isAlive
			def step    = stepOnTick(a1, a2, a3) {}
			def get     = f(a1.get, a2.get, a3.get)
			var tick    = a1.tick
		}
	}


	def lift[A1,A2,A3,A4,B](f: (A1,A2,A3,A4)=>B, da1: => Get[A1], da2: => Get[A2], da3: => Get[A3], da4: => Get[A4]): Get[B] = {
		lazy val a1 = da1
		lazy val a2 = da2		
		lazy val a3 = da3		
		lazy val a4 = da4

		new Get[B] {
			def isAlive = a1.isAlive && a2.isAlive && a3.isAlive && a4.isAlive
			def step    = stepOnTick(a1, a2, a3, a4) {}
			def get     = f(a1.get, a2.get, a3.get, a4.get)
			var tick    = a1.tick
		}
	}


	def lift[A1,A2,A3,A4,A5,B](f: (A1,A2,A3,A4,A5)=>B, da1: => Get[A1], da2: => Get[A2], da3: => Get[A3], da4: => Get[A4], da5: => Get[A5]): Get[B] = {
		lazy val a1 = da1
		lazy val a2 = da2		
		lazy val a3 = da3		
		lazy val a4 = da4
		lazy val a5 = da5

		new Get[B] {
			def isAlive = a1.isAlive && a2.isAlive && a3.isAlive && a4.isAlive && a5.isAlive
			def step    = stepOnTick(a1, a2, a3, a4, a5) {}
			def get     = f(a1.get, a2.get, a3.get, a4.get, a5.get)
			var tick    = a1.tick
		}
	}


}

object Evt {
	def switch[A](s0: Get[A], evts: Evt[Get[A]]): Get[A] = ???

	def apply[A](pred: => Boolean, value: => A): Evt[A] = Evt(new Get[Option[A]] with Alive with NoStep {
		def get = if (pred) Some(value) else None
	})
}

case class Evt[+A](stream: Get[Option[A]] = Get.const(None)) {

	def step[B >: A](s0: B): Get[B] = {
		def go(s: B, a: Option[A]) = a.getOrElse(s)

		Get.fsm(stream, s0, go, (x: B) => x)
	}

	def map[B](f: A => B): Evt[B] = 
		Evt(stream.map(_.map(f)))

	def append[B >: A](that: Evt[B]): Evt[B] = {
		def f(a: Option[B], b: Option[B]) = (a, b) match {
			case (Some(x), _) => Some(x)
			case (None,    x) => x
		} 

		Evt(Get.lift(f, stream, that.stream))
	}

	def getOrElse[B >: A](a: B): Get[B] = stream.map(_.getOrElse(a))

	def snapBy[B, C](f: (A, B) => C, bs: Get[B]): Evt[C] = Evt {
		def go(a: Option[A], b: B): Option[C] = a.map(x => f(x, b)) 

		Get.lift(go, stream, bs)
	}

	def react(f: A => Unit): Get[Unit] = 
		stream.map(_.map(f).getOrElse({}))
}

object Set {
	def const[A](s: A => Unit) = new Set[A] with Alive with NoStep {
		def set(a: A) = s(a)
	}

	def print = const(println)	

	def buffer[A](buffer: Buffer[A]): Set[A] = new Set[A] with Alive with NoStep {
		def set(a: A) {
			buffer += a
		}
	}
}

trait Runner extends Step with Being { self =>
	def run() = while (isAlive) {
		step
	}

	def append(that: Runner) = new Runner {
		def step = { self.step; that.step }
		var tick = self.tick
		def isAlive = self.isAlive || that.isAlive				
	}
}

/*
object Run {
	def apply(as: Seq[Runner]*): Runner = {

	}
}
*/

case class Run[A](setter: Set[A], getter: Get[A]) extends Runner {
	def isAlive = setter.isAlive && getter.isAlive 

	def step = if (isAlive) {
		setter.set(getter.get)
		getter.step
		setter.step
	} else {}	

	var tick = true
}
