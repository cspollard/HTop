#! /usr/bin/env python

from sys import stdout, argv
import yoda

analysisobjects_in = {}
for filename in argv[1:]:
    aos = yoda.read(filename)
    for aopath, ao in aos.iteritems():
        analysisobjects_in.setdefault(aopath, []).append(ao)


analysisobjects_out = {}
for p, aos in analysisobjects_in.iteritems():
    ao_out = aos[0].clone()
    for ao in aos[1:]:
        ao_out += ao

    analysisobjects_out[p] = ao_out

yoda.writeYODA(analysisobjects_out, stdout)
