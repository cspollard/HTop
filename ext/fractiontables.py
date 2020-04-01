import yoda
from sys import stdout

f = yoda.read("htop.yoda")

for s in f.values():
  path = s.annotation("Path")
  if not path.endswith("norm"):
    continue

  print(path)

  for p in s.points:
    wx = p.xMax - p.xMin
    
    wy = p.yMax - p.yMin
    y = p.y

    stdout.write("$%0.3f\t\\pm %0.3f$\t&\t" % (y*wx, wy*wx/2))

  print
