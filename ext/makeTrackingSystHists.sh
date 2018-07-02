tee TrackingSystHists.hs << EOF

{-# LANGUAGE OverloadedLists #-}

import Data.Histogram.Generic (Histogram, histogram)
import Data.Vector (Vector)
import Data.Histogram.Bin.Arb

EOF

for f in v2TRK_[BEFR]*.root; do python bfrag.py $f TrackingSystHists.hs; done
