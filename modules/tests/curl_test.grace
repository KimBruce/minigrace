import "io" as io
import "curl" as curl

def req = curl.easy
req.url := "http://web.cecs.pdx.edu/~grace/minigrace/exp/index.html"
print "req.url = {req.url}"
req.onReceive {d->
    print "data received"
    io.output.write(d.decode("utf-8"))
}
print "ready to perform ..."
req.perform
