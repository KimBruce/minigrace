import "ObjectTypeModule" as ot
import "SharedTypes" as share
def anObjectType : share.ObjectTypeFactory = ot.anObjectType

def newType = anObjectType.bottom | anObjectType.number | anObjectType.string | anObjectType.number
print ("new type is {newType}")