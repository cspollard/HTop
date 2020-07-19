import yoda
from sys import stdout

f = yoda.read("htop.yoda")

print("total uncertainties")

for s in f.values():
  path = s.annotation("Path")
  if not path.endswith("norm"):
    continue

  print(path)

  for p in s.points():
    wx = p.xMax() - p.xMin()

    wy = p.yMax() - p.yMin()
    y = p.y()

    stdout.write("\t&\t$%0.3f\t\\pm %0.3f$" % (y*wx, wy*wx/2))

  print("\t\\\\\n")

print("fractional uncertainties")

for s in f.values():
  path = s.annotation("Path")
  if not path.endswith("norm"):
    continue

  print(path)

  for p in s.points():
    wx = p.xMax() - p.xMin()

    wy = p.yMax() - p.yMin()
    y = p.y()

    stdout.write("\t&\t$%0.3f\t\\pm %04.1f$\\%%" % (y*wx, 100*wy / 2 / y))

  print("\t\\\\\n")
