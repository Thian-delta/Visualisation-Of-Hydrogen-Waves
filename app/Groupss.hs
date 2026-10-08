module Groupss
   (firstGroup, secondGroup, thirdGroup, fourthGroup, fifthGroup, sixthGroup, seventhGroup, eighthGroup, ninthGroup, tenthGroup,
    randomPoints1, randomPoints2, randomPoints3, randomPoints4, randomPoints5, randomPoints6, randomPoints7, randomPoints8, randomPoints9, randomPoints10,
    firstSubLevel1s, secondSubLevel1s, thirdSubLevel1s,
    firstSubLevel2s, secondSubLevel2s, thirdSubLevel2s, fourthSubLevel2s, fifthSubLevel2s, sixthSubLevel2s, seventhSubLevel2s, eighthSubLevel2s,
    firstSubLevel3s, secondSubLevel3s, thirdSubLevel3s, fourthSubLevel3s, fifthSubLevel3s, sixthSubLevel3s, seventhSubLevel3s, eighthSubLevel3s, ninthSubLevel3s, tenthSubLevel3s,
    firstSubLevel2px, secondSubLevel2px, thirdSubLevel2px, fourthSubLevel2px, fifthSubLevel2px,
    firstSubLevel2py, secondSubLevel2py, thirdSubLevel2py, fourthSubLevel2py, fifthSubLevel2py,
    firstSubLevel3px, secondSubLevel3px, thirdSubLevel3px, fourthSubLevel3px, fifthSubLevel3px, sixthSubLevel3px, seventhSubLevel3px, eighthSubLevel3px, ninthSubLevel3px, tenthSubLevel3px,
    firstSubLevel3py, secondSubLevel3py, thirdSubLevel3py, fourthSubLevel3py, fifthSubLevel3py, sixthSubLevel3py, seventhSubLevel3py, eighthSubLevel3py, ninthSubLevel3py, tenthSubLevel3py,
    secondSubLevel3dz2, thirdSubLevel3dz2, fourthSubLevel3dz2, fifthSubLevel3dz2, sixthSubLevel3dz2, seventhSubLevel3dz2, eighthSubLevel3dz2, ninthSubLevel3dz2, tenthSubLevel3dz2,
    secondSubLevel3dzx, thirdSubLevel3dzx, fourthSubLevel3dzx, fifthSubLevel3dzx, sixthSubLevel3dzx, seventhSubLevel3dzx, eighthSubLevel3dzx, ninthSubLevel3dzx, tenthSubLevel3dzx,
    secondSubLevel3dxy, thirdSubLevel3dxy, fourthSubLevel3dxy, fifthSubLevel3dxy, sixthSubLevel3dxy, seventhSubLevel3dxy, eighthSubLevel3dxy, ninthSubLevel3dxy, tenthSubLevel3dxy) where

import System.IO ()
import Graphics.Gloss
import System.Random
import HydrogenWaveFunction

--grouping of sublevels for different colors
firstGroup :: [Point] -> [Point]
firstGroup sublevel = [x|x <- sublevel, isRadius1 x]
   where isRadius1 :: Point -> Bool
         isRadius1 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) < 1     = True
                         |otherwise                              = False
secondGroup :: [Point] -> [Point]
secondGroup sublevel = [x|x <- sublevel, isRadius2 x]
   where isRadius2 :: Point -> Bool
         isRadius2 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) >= 1 && ((x/30)**2 + (y/30)**2)**(1/2) < 2     = True
                         |otherwise                                                                     = False
thirdGroup :: [Point] -> [Point]
thirdGroup sublevel = [x|x <- sublevel, isRadius3 x]
   where isRadius3 :: Point -> Bool
         isRadius3 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) >= 2 && ((x/30)**2 + (y/30)**2)**(1/2) < 3     = True
                         |otherwise                                                                     = False
fourthGroup :: [Point] -> [Point]
fourthGroup sublevel = [x|x <- sublevel, isRadius4 x]
   where isRadius4 :: Point -> Bool
         isRadius4 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) >= 3 && ((x/30)**2 + (y/30)**2)**(1/2) < 4     = True
                         |otherwise                                                                     = False
fifthGroup :: [Point] -> [Point]
fifthGroup sublevel = [x|x <- sublevel, isRadius5 x]
   where isRadius5 :: Point -> Bool
         isRadius5 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) >= 4 && ((x/30)**2 + (y/30)**2)**(1/2) < 5     = True
                         |otherwise                                                                     = False
sixthGroup :: [Point] -> [Point]
sixthGroup sublevel = [x|x <- sublevel, isRadius6 x]
   where isRadius6 :: Point -> Bool
         isRadius6 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) >= 5 && ((x/30)**2 + (y/30)**2)**(1/2) < 6     = True
                         |otherwise                                                                     = False
seventhGroup :: [Point] -> [Point]
seventhGroup sublevel = [x|x <- sublevel, isRadius7 x]
   where isRadius7 :: Point -> Bool
         isRadius7 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) >= 6 && ((x/30)**2 + (y/30)**2)**(1/2) < 7     = True
                         |otherwise                                                                     = False
eighthGroup :: [Point] -> [Point]
eighthGroup sublevel = [x|x <- sublevel, isRadius8 x]
   where isRadius8 :: Point -> Bool
         isRadius8 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) >= 7 && ((x/30)**2 + (y/30)**2)**(1/2) < 8     = True
                         |otherwise                                                                     = False
ninthGroup :: [Point] -> [Point]
ninthGroup sublevel = [x|x <- sublevel, isRadius9 x]
   where isRadius9 :: Point -> Bool
         isRadius9 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) >= 8 && ((x/30)**2 + (y/30)**2)**(1/2) < 9     = True
                         |otherwise                                                                     = False
tenthGroup :: [Point] -> [Point]
tenthGroup sublevel = [x|x <- sublevel, isRadius10 x]
   where isRadius10 :: Point -> Bool
         isRadius10 (x, y)|((x/30)**2 + (y/30)**2)**(1/2) >= 9    = True
                          |otherwise                              = False

--RANDOM POINTS
randomPoints1 :: [Point]
randomPoints1 = zip xs1 ys1
   where xs1 = take (length firstSubLevel1s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 32409
         ys1 = take (length firstSubLevel1s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 119048
randomPoints2 :: [Point]
randomPoints2 = zip xs2 ys2
   where xs2 = take (length secondSubLevel2px) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 4265
         ys2 = take (length secondSubLevel2px) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 8654342
randomPoints3 :: [Point]
randomPoints3 = zip xs3 ys3
   where xs3 = take (length thirdSubLevel2py) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 67356
         ys3 = take (length thirdSubLevel2py) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 8534
randomPoints4 :: [Point]
randomPoints4 = zip xs4 ys4
   where xs4 = take (length fourthSubLevel2s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 235
         ys4 = take (length fourthSubLevel2s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 3124
randomPoints5 :: [Point]
randomPoints5 = zip xs5 ys5
   where xs5 = take (length fifthSubLevel3dzx) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 587
         ys5 = take (length fifthSubLevel3dzx) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 24625
randomPoints6 :: [Point]
randomPoints6 = zip xs6 ys6
   where xs6 = take (length sixthSubLevel3dxy) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 243
         ys6 = take (length sixthSubLevel3dxy) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 7568
randomPoints7 :: [Point]
randomPoints7 = zip xs7 ys7
   where xs7 = take (length seventhSubLevel3px) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 32733
         ys7 = take (length seventhSubLevel3px) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 46342
randomPoints8 :: [Point]
randomPoints8 = zip xs8 ys8
   where xs8 = take (length eighthSubLevel3s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 2355
         ys8 = take (length eighthSubLevel3s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 8643
randomPoints9 :: [Point]
randomPoints9 = zip xs9 ys9
   where xs9 = take (length ninthSubLevel3s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 6486642
         ys9 = take (length ninthSubLevel3s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 86257
randomPoints10 :: [Point]
randomPoints10 = zip xs10 ys10
   where xs10 = take (length tenthSubLevel3s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 78963
         ys10 = take (length tenthSubLevel3s) $ randomRs ((-350.0)::Float, 350.0) $ mkStdGen 75246

--FIRST BIT (from scattered to sublevel1s formed)
--length firstGroup subLevel1s : 7173
firstSubLevel1s :: [Point]
firstSubLevel1s = firstGroup subLevel1s
--length secondGroup subLevel1s : 2523
secondSubLevel1s :: [Point]
secondSubLevel1s = secondGroup subLevel1s 
--length thirdGroup subLevel1s : 160
thirdSubLevel1s :: [Point]
thirdSubLevel1s = thirdGroup subLevel1s
--length fourthGroup subLevel1s : 0
--length fifthGroup subLevel1s : 0
--length sixthGroup subLevel1s : 0
--length seventhGroup subLevel1s : 0
--length eighthGroup subLevel1s : 0
--length ninthGroup subLevel1s : 0
--length tenthGroup subLevel1s : 0

--SECOND BIT (from sublevel1s to scattered to sublevel2s formed)
--length firstGroup subLevel2s : 499
firstSubLevel2s :: [Point]
firstSubLevel2s = firstGroup subLevel2s
--length secondGroup subLevel2s : 885
secondSubLevel2s :: [Point]
secondSubLevel2s = secondGroup subLevel2s 
--length thirdGroup subLevel2s : 3263
thirdSubLevel2s :: [Point]
thirdSubLevel2s = thirdGroup subLevel2s
--length fourthGroup subLevel2s : 2884
fourthSubLevel2s :: [Point]
fourthSubLevel2s = fourthGroup subLevel2s
--length fifthGroup subLevel2s : 1463
fifthSubLevel2s :: [Point]
fifthSubLevel2s = fifthGroup subLevel2s
--length sixthGroup subLevel2s : 549
sixthSubLevel2s :: [Point]
sixthSubLevel2s = sixthGroup subLevel2s
--length seventhGroup subLevel2s : 159
seventhSubLevel2s :: [Point]
seventhSubLevel2s = seventhGroup subLevel2s
--length eighthGroup subLevel2s : 25
eighthSubLevel2s :: [Point]
eighthSubLevel2s = eighthGroup subLevel2s
--length ninthGroup subLevel2s : 0
--length tenthGroup subLevel2s : 0

--THIRD BIT (from sublevel2s to scattered to sublevel3s formed)
--length firstGroup subLevel3s : 120
firstSubLevel3s :: [Point]
firstSubLevel3s = firstGroup subLevel3s
--length secondGroup subLevel3s : 282
secondSubLevel3s :: [Point]
secondSubLevel3s = secondGroup subLevel3s 
--length thirdGroup subLevel3s : 583
thirdSubLevel3s :: [Point]
thirdSubLevel3s = thirdGroup subLevel3s
--length fourthGroup subLevel3s : 72
fourthSubLevel3s :: [Point]
fourthSubLevel3s = fourthGroup subLevel3s
--length fifthGroup subLevel3s : 326
fifthSubLevel3s :: [Point]
fifthSubLevel3s = fifthGroup subLevel3s
--length sixthGroup subLevel3s : 1224
sixthSubLevel3s :: [Point]
sixthSubLevel3s = sixthGroup subLevel3s
--length seventhGroup subLevel3s : 1806
seventhSubLevel3s :: [Point]
seventhSubLevel3s = seventhGroup subLevel3s
--length eighthGroup subLevel3s : 1780
eighthSubLevel3s :: [Point]
eighthSubLevel3s = eighthGroup subLevel3s
--length ninthGroup subLevel3s : 1393
ninthSubLevel3s :: [Point]
ninthSubLevel3s = ninthGroup subLevel3s
--length tenthGroup subLevel3s : 944
tenthSubLevel3s :: [Point]
tenthSubLevel3s = tenthGroup subLevel3s

--FOURTH BIT (from sublevel3s to scattered to sublevel2px formed)
--length firstGroup subLevel2px: 399
firstSubLevel2px :: [Point]
firstSubLevel2px = firstGroup subLevel2px
--length secondGroup subLevel2px: 2776
secondSubLevel2px :: [Point]
secondSubLevel2px = secondGroup subLevel2px
--length thirdGroup subLevel2px: 3336
thirdSubLevel2px :: [Point]
thirdSubLevel2px = thirdGroup subLevel2px
--length fourthGroup subLevel2px : 2006
fourthSubLevel2px :: [Point]
fourthSubLevel2px = fourthGroup subLevel2px
--length fifthGroup subLevel2px : 834
fifthSubLevel2px :: [Point]
fifthSubLevel2px = fifthGroup subLevel2px
--length sixthGroup subLevel2px : 0
--length seventhGroup subLevel2px : 0
--length eighthGroup subLevel2px : 0
--length ninthGroup subLevel2px : 0
--length tenthGroup subLevel2px : 0

--FIFTH BIT (from sublevel2px to scattered to sublevel2py formed)
--length firstGroup subLevel2py: 400
firstSubLevel2py :: [Point]
firstSubLevel2py = firstGroup subLevel2py
--length secondGroup subLevel2py: 2770
secondSubLevel2py :: [Point]
secondSubLevel2py = secondGroup subLevel2py
--length thirdGroup subLevel2py: 3343
thirdSubLevel2py :: [Point]
thirdSubLevel2py = thirdGroup subLevel2py
--length fourthGroup subLevel2py : 2006
fourthSubLevel2py :: [Point]
fourthSubLevel2py = fourthGroup subLevel2py
--length fifthGroup subLevel2py : 832
fifthSubLevel2py :: [Point]
fifthSubLevel2py = fifthGroup subLevel2py
--length sixthGroup subLevel2py : 0
--length seventhGroup subLevel2py : 0
--length eighthGroup subLevel2py : 0
--length ninthGroup subLevel2py : 0
--length tenthGroup subLevel2py : 0

--SIXTH BIT (from sublevel2py to scattered to sublevel3px formed)
--length firstGroup subLevel3px: 133
firstSubLevel3px :: [Point]
firstSubLevel3px = firstGroup subLevel3px
--length secondGroup subLevel3px: 714
secondSubLevel3px :: [Point]
secondSubLevel3px = secondGroup subLevel3px
--length thirdGroup subLevel3px: 302
thirdSubLevel3px :: [Point]
thirdSubLevel3px = thirdGroup subLevel3px
--length fourthGroup subLevel3px : 108
fourthSubLevel3px :: [Point]
fourthSubLevel3px = fourthGroup subLevel3px
--length fifthGroup subLevel3px : 902
fifthSubLevel3px :: [Point]
fifthSubLevel3px = fifthGroup subLevel3px
--length sixthGroup subLevel3px : 1776
sixthSubLevel3px :: [Point]
sixthSubLevel3px = sixthGroup subLevel3px
--length seventhGroup subLevel3px : 2028
seventhSubLevel3px :: [Point]
seventhSubLevel3px = seventhGroup subLevel3px
--length eighthGroup subLevel3px : 1734
eighthSubLevel3px :: [Point]
eighthSubLevel3px = eighthGroup subLevel3px
--length ninthGroup subLevel3px : 1231
ninthSubLevel3px :: [Point]
ninthSubLevel3px = ninthGroup subLevel3px
--length tenthGroup subLevel3px : 773
tenthSubLevel3px :: [Point]
tenthSubLevel3px = tenthGroup subLevel3px

--SEVENTH BIT (from sublevel3px to scattered to sublevel3py formed)
--length firstGroup subLevel3py: 132
firstSubLevel3py :: [Point]
firstSubLevel3py = firstGroup subLevel3py
--length secondGroup subLevel3py: 716
secondSubLevel3py :: [Point]
secondSubLevel3py = secondGroup subLevel3py
--length thirdGroup subLevel3py: 301
thirdSubLevel3py :: [Point]
thirdSubLevel3py = thirdGroup subLevel3py
--length fourthGroup subLevel3py : 108
fourthSubLevel3py :: [Point]
fourthSubLevel3py = fourthGroup subLevel3py
--length fifthGroup subLevel3py : 901
fifthSubLevel3py :: [Point]
fifthSubLevel3py = fifthGroup subLevel3py
--length sixthGroup subLevel3py : 1783
sixthSubLevel3py :: [Point]
sixthSubLevel3py = sixthGroup subLevel3py
--length seventhGroup subLevel3py : 2024
seventhSubLevel3py :: [Point]
seventhSubLevel3py = seventhGroup subLevel3py
--length eighthGroup subLevel3py : 1732
eighthSubLevel3py :: [Point]
eighthSubLevel3py = eighthGroup subLevel3py
--length ninthGroup subLevel3py : 1231
ninthSubLevel3py :: [Point]
ninthSubLevel3py = ninthGroup subLevel3py
--length tenthGroup subLevel3py : 773
tenthSubLevel3py :: [Point]
tenthSubLevel3py = tenthGroup subLevel3py

--EIGHTH BIT (from sublevel3py to scattered to sublevel3dz2 formed)
--length firstGroup subLevel3dz2: 0
--length secondGroup subLevel3dz2: 125
secondSubLevel3dz2 :: [Point]
secondSubLevel3dz2 = secondGroup subLevel3dz2
--length thirdGroup subLevel3dz2: 715
thirdSubLevel3dz2 :: [Point]
thirdSubLevel3dz2 = thirdGroup subLevel3dz2
--length fourthGroup subLevel3dz2 : 1515
fourthSubLevel3dz2 :: [Point]
fourthSubLevel3dz2 = fourthGroup subLevel3dz2
--length fifthGroup subLevel3dz2 : 1948
fifthSubLevel3dz2 :: [Point]
fifthSubLevel3dz2 = fifthGroup subLevel3dz2
--length sixthGroup subLevel3dz2 : 1871
sixthSubLevel3dz2 :: [Point]
sixthSubLevel3dz2 = sixthGroup subLevel3dz2
--length seventhGroup subLevel3dz2 : 1439
seventhSubLevel3dz2 :: [Point]
seventhSubLevel3dz2 = seventhGroup subLevel3dz2
--length eighthGroup subLevel3dz2 : 974
eighthSubLevel3dz2 :: [Point]
eighthSubLevel3dz2 = eighthGroup subLevel3dz2
--length ninthGroup subLevel3dz2 : 577
ninthSubLevel3dz2 :: [Point]
ninthSubLevel3dz2 = ninthGroup subLevel3dz2
--length tenthGroup subLevel3dz2 : 317
tenthSubLevel3dz2 :: [Point]
tenthSubLevel3dz2 = tenthGroup subLevel3dz2

--NINTH BIT (from sublevel3dz2 to scattered to sublevel3dzx formed)
--length firstGroup subLevel3dzx: 0
--length secondGroup subLevel3dzx: 127
secondSubLevel3dzx :: [Point]
secondSubLevel3dzx = secondGroup subLevel3dzx
--length thirdGroup subLevel3dzx: 716
thirdSubLevel3dzx :: [Point]
thirdSubLevel3dzx = thirdGroup subLevel3dzx
--length fourthGroup subLevel3dzx : 1515
fourthSubLevel3dzx :: [Point]
fourthSubLevel3dzx = fourthGroup subLevel3dzx
--length fifthGroup subLevel3dzx : 1948
fifthSubLevel3dzx :: [Point]
fifthSubLevel3dzx = fifthGroup subLevel3dzx
--length sixthGroup subLevel3dzx : 1870
sixthSubLevel3dzx :: [Point]
sixthSubLevel3dzx = sixthGroup subLevel3dzx
--length seventhGroup subLevel3dzx : 1460
seventhSubLevel3dzx :: [Point]
seventhSubLevel3dzx = seventhGroup subLevel3dzx
--length eighthGroup subLevel3dzx : 979
eighthSubLevel3dzx :: [Point]
eighthSubLevel3dzx = eighthGroup subLevel3dzx
--length ninthGroup subLevel3dzx : 583
ninthSubLevel3dzx :: [Point]
ninthSubLevel3dzx = ninthGroup subLevel3dzx
--length tenthGroup subLevel3dzx : 334
tenthSubLevel3dzx :: [Point]
tenthSubLevel3dzx = tenthGroup subLevel3dzx

--TENTH BIT (from sublevel3dzx to scattered to sublevel3dxy formed)
--length firstGroup subLevel3dxy: 0
--length secondGroup subLevel3dxy: 125
secondSubLevel3dxy :: [Point]
secondSubLevel3dxy = secondGroup subLevel3dxy
--length thirdGroup subLevel3dxy: 724
thirdSubLevel3dxy :: [Point]
thirdSubLevel3dxy = thirdGroup subLevel3dxy
--length fourthGroup subLevel3dxy : 1522
fourthSubLevel3dxy :: [Point]
fourthSubLevel3dxy = fourthGroup subLevel3dxy
--length fifthGroup subLevel3dxy : 1939
fifthSubLevel3dxy :: [Point]
fifthSubLevel3dxy = fifthGroup subLevel3dxy
--length sixthGroup subLevel3dxy : 1880
sixthSubLevel3dxy :: [Point]
sixthSubLevel3dxy = sixthGroup subLevel3dxy
--length seventhGroup subLevel3dxy : 1453
seventhSubLevel3dxy :: [Point]
seventhSubLevel3dxy = seventhGroup subLevel3dxy
--length eighthGroup subLevel3dxy : 971
eighthSubLevel3dxy :: [Point]
eighthSubLevel3dxy = eighthGroup subLevel3dxy
--length ninthGroup subLevel3dxy : 585
ninthSubLevel3dxy :: [Point]
ninthSubLevel3dxy = ninthGroup subLevel3dxy
--length tenthGroup subLevel3dxy : 323
tenthSubLevel3dxy :: [Point]
tenthSubLevel3dxy = tenthGroup subLevel3dxy