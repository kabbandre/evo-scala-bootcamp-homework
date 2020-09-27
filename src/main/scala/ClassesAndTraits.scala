// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.

package ClassesAndTraits

class ClassesAndTraits extends App {
  sealed trait Shape[A] extends Located with Bounded with Movable[A]

  sealed trait Shape2D extends Shape[Shape2D] {
    def area: Double
  }

  sealed trait Shape3D extends Shape[Shape3D] with Located3D with Bounded3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Located3D {
    def z: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Bounded3D {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable[A] {
    def move(dx: Double, dy: Double, dz: Double): A
  }

  sealed trait Triangular3D {
    def slantLength: Double
  }

  final case class Point(x: Double, y: Double) extends Shape[Point] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double, dz: Double = 0): Point = Point(x + dx, x + dy)
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double)
      extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def move(dx: Double, dy: Double, dz: Double = 0): Circle = Circle(centerX + dx, centerY + dy, radius)
    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Rectangle(centerX: Double, centerY: Double, height: Double, width: Double) extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - width / 2
    override def maxX: Double = x + width / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def move(dx: Double, dy: Double, dz: Double = 0): Rectangle = Rectangle(centerX + dx, centerY + dy, height, width)
    override def area: Double = height * width
  }

  final case class Square(centerX: Double, centerY: Double, length: Double) extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - length / 2
    override def maxX: Double = x + length / 2
    override def minY: Double = y - length / 2
    override def maxY: Double = y + length / 2
    override def move(dx: Double, dy: Double, dz: Double = 0): Square = Square(centerX + dx, centerY + dy, length)
    override def area: Double = Math.pow(length, 2)
  }

  final case class Triangle(centerX: Double, centerY: Double, base: Double, height: Double) extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - base / 2
    override def maxX: Double = x + base / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def move(dx: Double, dy: Double, dz: Double = 0): Triangle = Triangle(centerX + dx, centerY + dy, base, height)
    override def area: Double = base * height / 2
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape[Point3D] with Bounded3D  {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(x + dx, y + dy, z + dz, radius)
    override def surfaceArea: Double = 4 * Math.PI * radius * 2
    override def volume: Double = (4/3) * Math.PI * Math.pow(radius, 3)
  }

  final case class Cube(x: Double, y: Double, z: Double, length: Double) extends Shape3D {
    override def minX: Double = x - length / 2
    override def maxX: Double = x + length / 2
    override def minY: Double = y - length / 2
    override def maxY: Double = y + length / 2
    override def minZ: Double = z - length / 2
    override def maxZ: Double = z + length / 2
    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(x + dx, y + dy, z + dz, length)
    override def surfaceArea: Double = 6 * Math.pow(length, 2)
    override def volume: Double = Math.pow(length, 3)
  }

  final case class Cuboid(x: Double, y: Double, z: Double, length: Double, width: Double, height: Double) extends Shape3D {
    override def minX: Double = x - length / 2
    override def maxX: Double = x + length / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def minZ: Double = z - width / 2
    override def maxZ: Double = z + width / 2
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(x + dx, y + dy, z + dz, length, width, height)
    override def surfaceArea: Double = 2 * (length * width + length * height + width * height)
    override def volume: Double = length * width * height
  }

  final case class Pyramid(x: Double, y: Double, z: Double, base: Square, height: Double) extends Shape3D with Triangular3D {
    override def slantLength: Double = Math.sqrt(Math.pow(base.length / 2, 2) + Math.pow(height, 2))
    override def minX: Double = x - base.length / 2
    override def maxX: Double = x + base.length / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def minZ: Double = z - base.length / 2
    override def maxZ: Double = z + base.length / 2
    override def move(dx: Double, dy: Double, dz: Double): Pyramid = Pyramid(x + dx, y + dy, z + dz, base, height)
    override def surfaceArea: Double = (1 / 2) * base.length * 4 * slantLength
    override def volume: Double = (1 / 3) / base.area * height
  }

  final case class Cone(x: Double, y: Double, z: Double, base: Circle, height: Double) extends Shape3D with Triangular3D {
    override def slantLength: Double = Math.sqrt(Math.pow(base.radius, 2) + Math.pow(height, 2))
    override def minX: Double = x - base.radius
    override def maxX: Double = x + base.radius
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def minZ: Double = z - base.radius
    override def maxZ: Double = z + base.radius
    override def move(dx: Double, dy: Double, dz: Double): Cone = Cone(x + dx, y + dy, z + dz, base, height)
    override def surfaceArea: Double = Math.PI * Math.pow(base.radius, 2) + Math.PI * slantLength * base.radius
    override def volume: Double = (1 / 3) * Math.PI * Math.pow(base.radius, 2) * height
  }
}
