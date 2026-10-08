module Main where

import System.IO ()
import Graphics.Gloss
import HydrogenWaveFunction ()
import Colorss
import Groupss
import Labelss

window :: Display
window = InWindow "hydrogen wave function" (700, 700) (0, 0)

move :: Float -> [Point] -> [Point] -> [Point]
move seconds x y|seconds <= 2        = [(x1+seconds*(x2-x1)/2, (y1+seconds*(y2-y1)/2))|((x1, y1), (x2, y2)) <- zip x y]
                |length x < length y = take (length x) y
                |otherwise           = y

--Function of movements when merged ((nth group) previously subLevela with col1, now subLevelb with col2)
merge :: Float -> Int -> [Float] -> [Float] -> [Point] -> [Point] -> [Picture]
merge seconds n col1 col2 nthSubLevela nthSubLevelb | length nthSubLevela >= length nthSubLevelb = map (particle (changeColor seconds col1 col2)) (move seconds (randomPoints n) nthSubLevelb)
                                                    | otherwise                                  = (map (particle (changeColor seconds col1 col2)) (move seconds (randomPoints n) (take (length nthSubLevela) nthSubLevelb))) ++
                                                                                                   (map (particle (changeColor seconds black1 col2)) (move seconds (drop (length nthSubLevela) (randomPoints n)) (drop (length nthSubLevela) nthSubLevelb)))
                                                            where randomPoints :: Int -> [Point]
                                                                  randomPoints x|x == 1 = randomPoints1
                                                                                |x == 2 = randomPoints2
                                                                                |x == 3 = randomPoints3
                                                                                |x == 4 = randomPoints4
                                                                                |x == 5 = randomPoints5
                                                                                |x == 6 = randomPoints6
                                                                                |x == 7 = randomPoints7
                                                                                |x == 8 = randomPoints8
                                                                                |x == 9 = randomPoints9
                                                                                |x == 10 = randomPoints10
                                                                                |otherwise = []

--Function of movements when scattered ((nth group) now subLevela with col1, later subLevelb)
scattered :: Float -> Int -> [Float] -> [Point] -> [Point] -> [Picture]
scattered seconds n col1 nthSubLevela nthSubLevelb | length nthSubLevela <= length nthSubLevelb = map (particle (theColor col1)) (move seconds nthSubLevela (randomPoints n))
                                                   | otherwise                                  = (map (particle (theColor col1)) (move seconds (take (length nthSubLevelb) nthSubLevela) (randomPoints n))) ++
                                                                                                  (map (particle (changeColor seconds col1 black1)) (move seconds (drop (length nthSubLevelb) nthSubLevela) (drop (length nthSubLevelb) (randomPoints n))))
                                                            where randomPoints :: Int -> [Point]
                                                                  randomPoints x|x == 1 = randomPoints1
                                                                                |x == 2 = randomPoints2
                                                                                |x == 3 = randomPoints3
                                                                                |x == 4 = randomPoints4
                                                                                |x == 5 = randomPoints5
                                                                                |x == 6 = randomPoints6
                                                                                |x == 7 = randomPoints7
                                                                                |x == 8 = randomPoints8
                                                                                |x == 9 = randomPoints9
                                                                                |x == 10 = randomPoints10
                                                                                |otherwise = []

animationFrame1 :: Float -> [Picture]
animationFrame1 seconds = (map (particle (changeColor seconds black1 grey8)) (take (length firstSubLevel1s) randomPoints1)) ++
                          (map (particle (changeColor seconds black1 grey8)) (take (length secondSubLevel1s) randomPoints2)) ++
                          (map (particle (changeColor seconds black1 grey10)) (take (length thirdSubLevel1s) randomPoints3))

animationFrame2 :: Float -> [Picture]
animationFrame2 seconds = (map (particle (theColor grey8)) (move seconds randomPoints1 firstSubLevel1s)) ++ 
                          (map (particle (theColor grey8)) (move seconds randomPoints2 secondSubLevel1s)) ++ 
                          (map (particle (theColor grey10)) (move seconds randomPoints3 thirdSubLevel1s))

animationFrame3 :: Float -> [Picture]
animationFrame3 seconds = scattered seconds 1 grey8 firstSubLevel1s firstSubLevel2s ++
                          scattered seconds 2 grey8 secondSubLevel1s secondSubLevel2s ++
                          scattered seconds 3 grey10 thirdSubLevel1s thirdSubLevel2s

animationFrame4 :: Float -> [Picture]
animationFrame4 seconds = merge seconds 1 grey8 pink3 firstSubLevel1s firstSubLevel2s ++
                          merge seconds 2 grey8 pink4 secondSubLevel1s secondSubLevel2s ++
                          merge seconds 3 grey10 pink5 thirdSubLevel1s thirdSubLevel2s ++
                          merge seconds 4 black1 pink6 [] fourthSubLevel2s ++
                          merge seconds 5 black1 pink7 [] fifthSubLevel2s ++
                          merge seconds 6 black1 pink8 [] sixthSubLevel2s ++
                          merge seconds 7 black1 pink9 [] seventhSubLevel2s ++
                          merge seconds 8 black1 pink10 [] eighthSubLevel2s

animationFrame5 :: Float -> [Picture]
animationFrame5 seconds = scattered seconds 1 pink3 firstSubLevel2s firstSubLevel3s ++
                          scattered seconds 2 pink4 secondSubLevel2s secondSubLevel3s ++
                          scattered seconds 3 pink5 thirdSubLevel2s thirdSubLevel3s ++
                          scattered seconds 4 pink6 fourthSubLevel2s fourthSubLevel3s ++
                          scattered seconds 5 pink7 fifthSubLevel2s fifthSubLevel3s ++
                          scattered seconds 6 pink8 sixthSubLevel2s sixthSubLevel3s ++
                          scattered seconds 7 pink9 seventhSubLevel2s seventhSubLevel3s ++
                          scattered seconds 8 pink10 eighthSubLevel2s eighthSubLevel3s

animationFrame6 :: Float -> [Picture]
animationFrame6 seconds = merge seconds 1 pink3 purple1 firstSubLevel2s firstSubLevel3s ++
                          merge seconds 2 pink4 purple2 secondSubLevel2s secondSubLevel3s ++
                          merge seconds 3 pink5 purple3 thirdSubLevel2s thirdSubLevel3s ++
                          merge seconds 4 pink6 purple4 fourthSubLevel2s fourthSubLevel3s ++
                          merge seconds 5 pink7 purple5 fifthSubLevel2s fifthSubLevel3s ++
                          merge seconds 6 pink8 purple6 sixthSubLevel2s sixthSubLevel3s ++
                          merge seconds 7 pink9 purple7 seventhSubLevel2s seventhSubLevel3s ++
                          merge seconds 8 pink10 purple8 eighthSubLevel2s eighthSubLevel3s ++
                          merge seconds 9 black1 purple9 [] ninthSubLevel3s ++
                          merge seconds 10 black1 purple10 [] tenthSubLevel3s

animationFrame7 :: Float -> [Picture]
animationFrame7 seconds = scattered seconds 1 purple1 firstSubLevel3s firstSubLevel2px ++
                          scattered seconds 2 purple2 secondSubLevel3s secondSubLevel2px ++
                          scattered seconds 3 purple3 thirdSubLevel3s thirdSubLevel2px ++
                          scattered seconds 4 purple4 fourthSubLevel3s fourthSubLevel2px ++
                          scattered seconds 5 purple5 fifthSubLevel3s fifthSubLevel2px ++
                          scattered seconds 6 purple6 sixthSubLevel3s [] ++
                          scattered seconds 7 purple7 seventhSubLevel3s [] ++
                          scattered seconds 8 purple8 eighthSubLevel3s [] ++
                          scattered seconds 9 purple9 ninthSubLevel3s [] ++
                          scattered seconds 10 purple10 tenthSubLevel3s []

animationFrame8 :: Float -> [Picture]
animationFrame8 seconds = merge seconds 1 purple1 blue22 firstSubLevel3s firstSubLevel2px ++
                          merge seconds 2 purple2 blue24 secondSubLevel3s secondSubLevel2px ++
                          merge seconds 3 purple3 blue26 thirdSubLevel3s thirdSubLevel2px ++
                          merge seconds 4 purple4 blue28 fourthSubLevel3s fourthSubLevel2px ++
                          merge seconds 5 purple5 blue210 fifthSubLevel3s fifthSubLevel2px

animationFrame9 :: Float -> [Picture]
animationFrame9 seconds = scattered seconds 1 blue22 firstSubLevel2px firstSubLevel2py ++
                          scattered seconds 2 blue24 secondSubLevel2px secondSubLevel2py ++
                          scattered seconds 3 blue26 thirdSubLevel2px thirdSubLevel2py ++
                          scattered seconds 4 blue28 fourthSubLevel2px fourthSubLevel2py ++
                          scattered seconds 5 blue210 fifthSubLevel2px fifthSubLevel2py

animationFrame10 :: Float -> [Picture]
animationFrame10 seconds = merge seconds 1 blue22 blue12 firstSubLevel2px firstSubLevel2py ++
                           merge seconds 2 blue24 blue14 secondSubLevel2px secondSubLevel2py ++
                           merge seconds 3 blue26 blue16 thirdSubLevel2px thirdSubLevel2py ++
                           merge seconds 4 blue28 blue18 fourthSubLevel2px fourthSubLevel2py ++
                           merge seconds 5 blue210 blue110 fifthSubLevel2px fifthSubLevel2py

animationFrame11 :: Float -> [Picture]
animationFrame11 seconds = scattered seconds 1 blue12 firstSubLevel2py firstSubLevel3px ++
                           scattered seconds 2 blue14 secondSubLevel2py secondSubLevel3px ++
                           scattered seconds 3 blue16 thirdSubLevel2py thirdSubLevel3px ++
                           scattered seconds 4 blue18 fourthSubLevel2py fourthSubLevel3px ++
                           scattered seconds 5 blue110 fifthSubLevel2py fifthSubLevel3px

animationFrame12 :: Float -> [Picture]
animationFrame12 seconds = merge seconds 1 blue12 green21 firstSubLevel2py firstSubLevel3px ++
                           merge seconds 2 blue14 green22 secondSubLevel2py secondSubLevel3px ++
                           merge seconds 3 blue16 green23 thirdSubLevel2py thirdSubLevel3px ++
                           merge seconds 4 blue18 green24 fourthSubLevel2py fourthSubLevel3px ++
                           merge seconds 5 blue110 green25 fifthSubLevel2py fifthSubLevel3px ++
                           merge seconds 6 black1 green26 [] sixthSubLevel3px ++
                           merge seconds 7 black1 green27 [] seventhSubLevel3px ++
                           merge seconds 8 black1 green28 [] eighthSubLevel3px ++
                           merge seconds 9 black1 green29 [] ninthSubLevel3px ++
                           merge seconds 10 black1 green210 [] tenthSubLevel3px

animationFrame13 :: Float -> [Picture]
animationFrame13 seconds = scattered seconds 1 green21 firstSubLevel3px firstSubLevel3py ++
                           scattered seconds 2 green22 secondSubLevel3px secondSubLevel3py ++
                           scattered seconds 3 green23 thirdSubLevel3px thirdSubLevel3py ++
                           scattered seconds 4 green24 fourthSubLevel3px fourthSubLevel3py ++
                           scattered seconds 5 green25 fifthSubLevel3px fifthSubLevel3py ++
                           scattered seconds 6 green26 sixthSubLevel3px sixthSubLevel3py ++
                           scattered seconds 7 green27 seventhSubLevel3px seventhSubLevel3py ++
                           scattered seconds 8 green28 eighthSubLevel3px eighthSubLevel3py ++
                           scattered seconds 9 green29 ninthSubLevel3px ninthSubLevel3py ++
                           scattered seconds 10 green210 tenthSubLevel3px tenthSubLevel3py

animationFrame14 :: Float -> [Picture]
animationFrame14 seconds = merge seconds 1 green21 green11 firstSubLevel3px firstSubLevel3py ++
                           merge seconds 2 green22 green12 secondSubLevel3px secondSubLevel3py ++
                           merge seconds 3 green23 green13 thirdSubLevel3px thirdSubLevel3py ++
                           merge seconds 4 green24 green14 fourthSubLevel3px fourthSubLevel3py ++
                           merge seconds 5 green25 green15 fifthSubLevel3px fifthSubLevel3py ++
                           merge seconds 6 green26 green16 sixthSubLevel3px sixthSubLevel3py ++
                           merge seconds 7 green27 green17 seventhSubLevel3px seventhSubLevel3py ++
                           merge seconds 8 green28 green18 eighthSubLevel3px eighthSubLevel3py ++
                           merge seconds 9 green29 green19 ninthSubLevel3px ninthSubLevel3py ++
                           merge seconds 10 green210 green110 tenthSubLevel3px tenthSubLevel3py

animationFrame15 :: Float -> [Picture]
animationFrame15 seconds = scattered seconds 1 green11 firstSubLevel3py [] ++
                           scattered seconds 2 green12 secondSubLevel3py secondSubLevel3dz2 ++
                           scattered seconds 3 green13 thirdSubLevel3py thirdSubLevel3dz2 ++
                           scattered seconds 4 green14 fourthSubLevel3py fourthSubLevel3dz2 ++
                           scattered seconds 5 green15 fifthSubLevel3py fifthSubLevel3dz2 ++
                           scattered seconds 6 green16 sixthSubLevel3py sixthSubLevel3dz2 ++
                           scattered seconds 7 green17 seventhSubLevel3py seventhSubLevel3dz2 ++
                           scattered seconds 8 green18 eighthSubLevel3py eighthSubLevel3dz2 ++
                           scattered seconds 9 green19 ninthSubLevel3py ninthSubLevel3dz2 ++
                           scattered seconds 10 green110 tenthSubLevel3py tenthSubLevel3dz2                       

animationFrame16 :: Float -> [Picture]
animationFrame16 seconds = merge seconds 2 green12 yellow1 secondSubLevel3py secondSubLevel3dz2 ++
                           merge seconds 3 green13 yellow3 thirdSubLevel3py thirdSubLevel3dz2 ++
                           merge seconds 4 green14 yellow4 fourthSubLevel3py fourthSubLevel3dz2 ++
                           merge seconds 5 green15 yellow5 fifthSubLevel3py fifthSubLevel3dz2 ++
                           merge seconds 6 green16 yellow6 sixthSubLevel3py sixthSubLevel3dz2 ++
                           merge seconds 7 green17 yellow7 seventhSubLevel3py seventhSubLevel3dz2 ++
                           merge seconds 8 green18 yellow8 eighthSubLevel3py eighthSubLevel3dz2 ++
                           merge seconds 9 green19 yellow9 ninthSubLevel3py ninthSubLevel3dz2 ++
                           merge seconds 10 green110 yellow10 tenthSubLevel3py tenthSubLevel3dz2

animationFrame17 :: Float -> [Picture]
animationFrame17 seconds = scattered seconds 2 yellow1 secondSubLevel3dz2 secondSubLevel3dzx ++
                           scattered seconds 3 yellow3 thirdSubLevel3dz2 thirdSubLevel3dzx ++
                           scattered seconds 4 yellow4 fourthSubLevel3dz2 fourthSubLevel3dzx ++
                           scattered seconds 5 yellow5 fifthSubLevel3dz2 fifthSubLevel3dzx ++
                           scattered seconds 6 yellow6 sixthSubLevel3dz2 sixthSubLevel3dzx ++
                           scattered seconds 7 yellow7 seventhSubLevel3dz2 seventhSubLevel3dzx ++
                           scattered seconds 8 yellow8 eighthSubLevel3dz2 eighthSubLevel3dzx ++
                           scattered seconds 9 yellow9 ninthSubLevel3dz2 ninthSubLevel3dzx ++
                           scattered seconds 10 yellow10 tenthSubLevel3dz2 tenthSubLevel3dzx

animationFrame18 :: Float -> [Picture]
animationFrame18 seconds = merge seconds 2 yellow1 orange1 secondSubLevel3dz2 secondSubLevel3dzx ++
                           merge seconds 3 yellow3 orange3 thirdSubLevel3dz2 thirdSubLevel3dzx ++
                           merge seconds 4 yellow4 orange4 fourthSubLevel3dz2 fourthSubLevel3dzx ++
                           merge seconds 5 yellow5 orange5 fifthSubLevel3dz2 fifthSubLevel3dzx ++
                           merge seconds 6 yellow6 orange6 sixthSubLevel3dz2 sixthSubLevel3dzx ++
                           merge seconds 7 yellow7 orange7 seventhSubLevel3dz2 seventhSubLevel3dzx ++
                           merge seconds 8 yellow8 orange8 eighthSubLevel3dz2 eighthSubLevel3dzx ++
                           merge seconds 9 yellow9 orange9 ninthSubLevel3dz2 ninthSubLevel3dzx ++
                           merge seconds 10 yellow10 orange10 tenthSubLevel3dz2 tenthSubLevel3dzx

animationFrame19 :: Float -> [Picture]
animationFrame19 seconds = scattered seconds 2 orange1 secondSubLevel3dzx secondSubLevel3dxy ++
                           scattered seconds 3 orange3 thirdSubLevel3dzx thirdSubLevel3dxy ++
                           scattered seconds 4 orange4 fourthSubLevel3dzx fourthSubLevel3dxy ++
                           scattered seconds 5 orange5 fifthSubLevel3dzx fifthSubLevel3dxy ++
                           scattered seconds 6 orange6 sixthSubLevel3dzx sixthSubLevel3dxy ++
                           scattered seconds 7 orange7 seventhSubLevel3dzx seventhSubLevel3dxy ++
                           scattered seconds 8 orange8 eighthSubLevel3dzx eighthSubLevel3dxy ++
                           scattered seconds 9 orange9 ninthSubLevel3dzx ninthSubLevel3dxy ++
                           scattered seconds 10 orange10 tenthSubLevel3dzx tenthSubLevel3dxy

animationFrame20 :: Float -> [Picture]
animationFrame20 seconds = merge seconds 2 orange1 red1 secondSubLevel3dzx secondSubLevel3dxy ++
                           merge seconds 3 orange3 red3 thirdSubLevel3dzx thirdSubLevel3dxy ++
                           merge seconds 4 orange4 red4 fourthSubLevel3dzx fourthSubLevel3dxy ++
                           merge seconds 5 orange5 red5 fifthSubLevel3dzx fifthSubLevel3dxy ++
                           merge seconds 6 orange6 red6 sixthSubLevel3dzx sixthSubLevel3dxy ++
                           merge seconds 7 orange7 red7 seventhSubLevel3dzx seventhSubLevel3dxy ++
                           merge seconds 8 orange8 red8 eighthSubLevel3dzx eighthSubLevel3dxy ++
                           merge seconds 9 orange9 red9 ninthSubLevel3dzx ninthSubLevel3dxy ++
                           merge seconds 10 orange10 red10 tenthSubLevel3dzx tenthSubLevel3dxy 

animationFrame21 :: Float -> [Picture]
animationFrame21 seconds = scattered seconds 2 red1 secondSubLevel3dxy [] ++
                           scattered seconds 3 red3 thirdSubLevel3dxy [] ++
                           scattered seconds 4 red4 fourthSubLevel3dxy [] ++
                           scattered seconds 5 red5 fifthSubLevel3dxy [] ++
                           scattered seconds 6 red6 sixthSubLevel3dxy [] ++
                           scattered seconds 7 red7 seventhSubLevel3dxy [] ++
                           scattered seconds 8 red8 eighthSubLevel3dxy [] ++
                           scattered seconds 9 red9 ninthSubLevel3dxy [] ++
                           scattered seconds 10 red10 tenthSubLevel3dxy []

--Labelss
labelMerge :: Float -> [Float] -> [Point] -> [Picture]
labelMerge seconds col labelSubLevela = (map (thickParticle (changeColor seconds black1 col)) (move seconds randomPointsforLabels labelSubLevela))
labelScattered :: Float -> [Float] -> [Point] -> [Picture]
labelScattered seconds col labelSubLevela = (map (thickParticle (changeColor seconds col black1)) (move seconds labelSubLevela randomPointsforLabels))

animationLabel1 :: Float -> [Picture]
animationLabel1 seconds = labelMerge seconds grey10 labelSubLevel1s
animationLabel2 :: Float -> [Picture]
animationLabel2 seconds = labelScattered seconds grey10 labelSubLevel1s
animationLabel3 :: Float -> [Picture]
animationLabel3 seconds = labelMerge seconds pink10 labelSubLevel2s
animationLabel4 :: Float -> [Picture]
animationLabel4 seconds = labelScattered seconds pink10 labelSubLevel2s
animationLabel5 :: Float -> [Picture]
animationLabel5 seconds = labelMerge seconds purple10 labelSubLevel3s
animationLabel6 :: Float -> [Picture]
animationLabel6 seconds = labelScattered seconds purple10 labelSubLevel3s
animationLabel7 :: Float -> [Picture]
animationLabel7 seconds = labelMerge seconds blue210 labelSubLevel2px
animationLabel8 :: Float -> [Picture]
animationLabel8 seconds = labelScattered seconds blue210 labelSubLevel2px
animationLabel9 :: Float -> [Picture]
animationLabel9 seconds = labelMerge seconds blue110 labelSubLevel2py
animationLabel10 :: Float -> [Picture]
animationLabel10 seconds = labelScattered seconds blue110 labelSubLevel2py
animationLabel11 :: Float -> [Picture]
animationLabel11 seconds = labelMerge seconds green210 labelSubLevel3px
animationLabel12 :: Float -> [Picture]
animationLabel12 seconds = labelScattered seconds green210 labelSubLevel3px
animationLabel13 :: Float -> [Picture]
animationLabel13 seconds = labelMerge seconds green110 labelSubLevel3py
animationLabel14 :: Float -> [Picture]
animationLabel14 seconds = labelScattered seconds green110 labelSubLevel3py
animationLabel15 :: Float -> [Picture]
animationLabel15 seconds = labelMerge seconds yellow10 labelSubLevel3dz2
animationLabel16 :: Float -> [Picture]
animationLabel16 seconds = labelScattered seconds yellow10 labelSubLevel3dz2
animationLabel17 :: Float -> [Picture]
animationLabel17 seconds = labelMerge seconds orange10 labelSubLevel3dzx
animationLabel18 :: Float -> [Picture]
animationLabel18 seconds = labelScattered seconds orange10 labelSubLevel3dzx
animationLabel19 :: Float -> [Picture]
animationLabel19 seconds = labelMerge seconds red10 labelSubLevel3dxy
animationLabel20 :: Float -> [Picture]
animationLabel20 seconds = labelScattered seconds red10 labelSubLevel3dxy


fullAnimations :: Float -> [Picture]
fullAnimations seconds|seconds <= 2  = [Blank]
                      |seconds <= 3  = animationFrame1 ((seconds - 2)*2)
                      |seconds <= 6  = animationFrame2 (seconds - 3) ++ animationLabel1 ((seconds - 3)*2)
                      |seconds <= 8 = animationFrame3 (seconds - 6) ++ animationLabel2 ((seconds - 6)*2)
                      |seconds <= 11 = animationFrame4 (seconds - 8) ++ animationLabel3 ((seconds - 8)*2)
                      |seconds <= 13 = animationFrame5 (seconds - 11) ++ animationLabel4 ((seconds - 11)*2)
                      |seconds <= 16 = animationFrame6 (seconds - 13) ++ animationLabel5 ((seconds - 13)*2)
                      |seconds <= 18 = animationFrame7 (seconds - 16) ++ animationLabel6 ((seconds - 16)*2)
                      |seconds <= 21 = animationFrame8 (seconds - 18) ++ animationLabel7 ((seconds - 18)*2)
                      |seconds <= 23 = animationFrame9 (seconds - 21) ++ animationLabel8 ((seconds - 21)*2)
                      |seconds <= 26 = animationFrame10 (seconds - 23) ++ animationLabel9 ((seconds - 23)*2)
                      |seconds <= 28 = animationFrame11 (seconds - 26) ++ animationLabel10 ((seconds - 26)*2)
                      |seconds <= 31 = animationFrame12 (seconds - 28) ++ animationLabel11 ((seconds - 28)*2)
                      |seconds <= 33 = animationFrame13 (seconds - 31) ++ animationLabel12 ((seconds - 31)*2)
                      |seconds <= 36 = animationFrame14 (seconds - 33) ++ animationLabel13 ((seconds - 33)*2)
                      |seconds <= 38 = animationFrame15 (seconds - 36) ++ animationLabel14 ((seconds - 36)*2)
                      |seconds <= 41 = animationFrame16 (seconds - 38) ++ animationLabel15 ((seconds - 38)*2)
                      |seconds <= 43 = animationFrame17 (seconds - 41) ++ animationLabel16 ((seconds - 41)*2)
                      |seconds <= 46 = animationFrame18 (seconds - 43) ++ animationLabel17 ((seconds - 43)*2)
                      |seconds <= 48 = animationFrame19 (seconds - 46) ++ animationLabel18 ((seconds - 46)*2)
                      |seconds <= 51 = animationFrame20 (seconds - 48) ++ animationLabel19 ((seconds - 48)*2)
                      |seconds <= 53 = animationFrame21 ((seconds - 51)*2) ++ animationLabel20 ((seconds - 51)*2)
                      |otherwise     = [Blank]

frame :: Float -> Picture
frame seconds = pictures (fullAnimations seconds)

main :: IO ()
main = animate window black frame
