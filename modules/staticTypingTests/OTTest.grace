import "ObjectTypeModule" as ot
import "SharedTypes" as sh
type ObjectType = sh.ObjectType
def anObjectType: sh.ObjectTypeFactory = ot.anObjectType
def number: ObjectType = anObjectType.number
def point: ObjectType = anObjectType.point
print (anObjectType.point)
print (anObjectType.number)
def andType: ObjectType = anObjectType.point & anObjectType.number
def orType: ObjectType = anObjectType.point | anObjectType.number
print ("\npoint & number => \n{andType}")
print ("\npoint | number  => \n{orType}")
def suba: Boolean = andType.isSubtypeOf (number)
def subb: Boolean = andType.isSubtypeOf (point)
def subc: Boolean = number.isSubtypeOf (orType)
def subd: Boolean = andType.isSubtypeOf (orType)

def sube: Boolean = orType.isSubtypeOf (number)
def subf: Boolean = number.isSubtypeOf (andType)
def subg: Boolean = number.isSubtypeOf (point)
print "Following should all be true"
print ("{suba} {subb} {subc} {subd}")
print "Following should all be false"
print ("{sube} {subf} {subg} ")
