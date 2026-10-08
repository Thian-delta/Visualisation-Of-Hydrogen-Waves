module Colorss
    (particle, thickParticle, 
     red1, red2, red3, red4, red5, red6, red7, red8, red9, red10,
     orange1, orange2, orange3, orange4, orange5, orange6, orange7, orange8, orange9, orange10,
     yellow1, yellow2, yellow3, yellow4, yellow5, yellow6, yellow7, yellow8, yellow9, yellow10,
     green11, green12, green13, green14, green15, green16, green17, green18, green19, green110,
     green21, green22, green23, green24, green25, green26, green27, green28, green29, green210,
     blue11, blue12, blue13, blue14, blue15, blue16, blue17, blue18, blue19, blue110,
     blue21, blue22, blue23, blue24, blue25, blue26, blue27, blue28, blue29, blue210,
     purple1, purple2, purple3, purple4, purple5, purple6, purple7, purple8, purple9, purple10,
     pink1, pink2, pink3, pink4, pink5, pink6, pink7, pink8, pink9, pink10,
     grey1, grey2, grey3, grey4, grey5, grey6, grey7, grey8, grey9, grey10,
     black1, white1, theColor, changeColor) where

import System.IO ()
import Graphics.Gloss
import System.Random()

--a particle with color color, position x and y, radius 1
particle :: Color -> Point -> Picture
particle col (x, y) = Color col $ translate x y $ circleSolid 1.0
--a particle with color color, position x and y, radius 2
thickParticle :: Color -> Point -> Picture
thickParticle col (x, y) = Color col $ translate x y $ circleSolid 2.0

--red!!!
red1 :: [Float]
red1 = [(200/255), 0, 0, 1]
red2 :: [Float]
red2 = [(214/255), 0, 0, 1]
red3 :: [Float]
red3 = [(238/255), 0, 0, 1]
red4 :: [Float]
red4 = [(255/255), (1/255), (1/255), 1]
red5 :: [Float]
red5 = [(255/255), (25/255), (25/255), 1]
red6 :: [Float]
red6 = [(255/255), (50/255), (50/255), 1]
red7 :: [Float]
red7 = [(255/255), (75/255), (75/255), 1]
red8 :: [Float]
red8 = [(255/255), (100/255), (100/255), 1]
red9 :: [Float]
red9 = [(255/255), (125/255), (125/255), 1]
red10 :: [Float]
red10 = [(255/255), (150/255), (150/255), 1]

--orange!!
orange1 :: [Float]
orange1 = [(168/255), (92/255), 0, 1]
orange2 :: [Float]
orange2 = [(184/255), (101/255), 0, 1]
orange3 :: [Float]
orange3 = [(200/255), (110/255), 0, 1]
orange4 :: [Float]
orange4 = [(214/255), (117/255), 0, 1]
orange5 :: [Float]
orange5 = [(238/255), (130/255), 0, 1]
orange6 :: [Float]
orange6 = [(255/255), (140/255), (1/255), 1]
orange7 :: [Float]
orange7 = [(255/255), (165/255), (55/255), 1]
orange8 :: [Float]
orange8 = [(255/255), (190/255), (111/255), 1]
orange9 :: [Float]
orange9 = [(255/255), (215/255), (167/255), 1]
orange10 :: [Float]
orange10 = [(255/255), (222/255), (185/255), 1]

--yellow!!
yellow1 :: [Float]
yellow1 = [(200/255), (176/255), (1/255), 1]
yellow2 :: [Float]
yellow2 = [(210/255), (185/255), (1/255), 1]
yellow3 :: [Float]
yellow3 = [(220/255), (194/255), (1/255), 1]
yellow4 :: [Float]
yellow4 = [(230/255), (203/255), (1/255), 1]
yellow5 :: [Float]
yellow5 = [(240/255), (211/255), (1/255), 1]
yellow6 :: [Float]
yellow6 = [(250/255), (220/255), (1/255), 1]
yellow7 :: [Float]
yellow7 = [(254/255), (225/255), (10/255), 1]
yellow8 :: [Float]
yellow8 = [(254/255), (230/255), (52/255), 1]
yellow9 :: [Float]
yellow9 = [(254/255), (236/255), (102/255), 1]
yellow10 :: [Float]
yellow10 = [(254/255), (238/255), (122/255), 1]

--green1!!
green11 :: [Float]
green11 = [(50/255), (110/255), 0, 1]
green12 :: [Float]
green12 = [(76/255), (168/255), 0, 1]
green13 :: [Float]
green13 = [(100/255), (222/255), 0, 1]
green14 :: [Float]
green14 = [(120/255), (255/255), (9/255), 1]
green15 :: [Float]
green15 = [(130/255), (255/255), (27/255), 1]
green16 :: [Float]
green16 = [(140/255), (255/255), (45/255), 1]
green17 :: [Float]
green17 = [(163/255), (255/255), (87/255), 1]
green18 :: [Float]
green18 = [(186/255), (255/255), (129/255), 1]
green19 :: [Float]
green19 = [(207/255), (255/255), (167/255), 1]
green110 :: [Float]
green110 = [(228/255), (255/255), (205/255), 1]

--green2!!
green21 :: [Float]
green21 = [0, (130/255), (50/255), 1]
green22 :: [Float]
green22 = [0, (168/255), (64/255), 1]
green23 :: [Float]
green23 = [0, (204/255), (78/255), 1]
green24 :: [Float]
green24 = [0, (230/255), (88/255), 1]
green25 :: [Float]
green25 = [(1/255), (255/255), (98/255), 1]
green26 :: [Float]
green26 = [(29/255), (255/255), (115/255), 1]
green27 :: [Float]
green27 = [(91/255), (255/255), (153/255), 1]
green28 :: [Float]
green28 = [(151/255), (255/255), (191/255), 1]
green29 :: [Float]
green29 = [(185/255), (255/255), (212/255), 1]
green210 :: [Float]
green210 = [(221/255), (255/255), (234/255), 1]

--blue1!!
blue11 :: [Float]
blue11 = [0, (142/255), (118/255), 1]
blue12 :: [Float]
blue12 = [0, (172/255), (143/255), 1]
blue13 :: [Float]
blue13 = [0, (200/255), (167/255), 1]
blue14 :: [Float]
blue14 = [0, (226/255), (188/255), 1]
blue15 :: [Float]
blue15 = [0, (254/255), (212/255), 1]
blue16 :: [Float]
blue16 = [(93/255), (255/255), (228/255), 1]
blue17 :: [Float]
blue17 = [(125/255), (255/255), (233/255), 1]
blue18 :: [Float]
blue18 = [(155/255), (255/255), (238/255), 1]
blue19 :: [Float]
blue19 = [(179/255), (255/255), (242/255), 1]
blue110 :: [Float]
blue110 = [(205/255), (255/255), (247/255), 1]

--blue2!!
blue21 :: [Float]
blue21 = [0, (100/255), (127/255), 1]
blue22 :: [Float]
blue22 = [0, (130/255), (166/255), 1]
blue23 :: [Float]
blue23 = [0, (145/255), (184/255), 1]
blue24 :: [Float]
blue24 = [0, (160/255), (204/255), 1]
blue25 :: [Float]
blue25 = [0, (174/255), (222/255), 1]
blue26 :: [Float]
blue26 = [0, (190/255), (242/255), 1]
blue27 :: [Float]
blue27 = [(21/255), (205/255), (255/255), 1]
blue28 :: [Float]
blue28 = [(113/255), (225/255), (255/255), 1]
blue29 :: [Float]
blue29 = [(171/255), (237/255), (255/255), 1]
blue210 :: [Float]
blue210 = [(209/255), (245/255), (255/255), 1]

--purple!!
purple1 :: [Float]
purple1 = [(84/255), 0, (122/255), 1]
purple2 :: [Float]
purple2 = [(119/255), 0, (172/255), 1]
purple3 :: [Float]
purple3 = [(145/255), 0, (210/255), 1]
purple4 :: [Float]
purple4 = [(164/255), 0, (238/255), 1]
purple5 :: [Float]
purple5 = [(179/255), (9/255), (255/255), 1]
purple6 :: [Float]
purple6 = [(193/255), (55/255), (255/255), 1]
purple7 :: [Float]
purple7 = [(210/255), (109/255), (255/255), 1]
purple8 :: [Float]
purple8 = [(222/255), (147/255), (255/255), 1]
purple9 :: [Float]
purple9 = [(230/255), (175/255), (255/255), 1]
purple10 :: [Float]
purple10 = [(240/255), (205/255), (255/255), 1]

--pink!!
pink1 :: [Float]
pink1 = [(54/255), 0, (50/255), 1]
pink2 :: [Float]
pink2 = [(158/255), 0, (147/255), 1]
pink3 :: [Float]
pink3 = [(184/255), 0, (171/255), 1]
pink4 :: [Float]
pink4 = [(208/255), 0, (193/255), 1]
pink5 :: [Float]
pink5 = [(234/255), 0, (217/255), 1]
pink6 :: [Float]
pink6 = [(255/255), (71/255), (242/255), 1]
pink7 :: [Float]
pink7 = [(255/255), (109/255), (245/255), 1]
pink8 :: [Float]
pink8 = [(255/255), (179/255), (250/255), 1]
pink9 :: [Float]
pink9 = [(255/255), (201/255), (251/255), 1]
pink10 :: [Float]
pink10 = [(255/255), (235/255), (254/255), 1]

--grey!!
grey1 :: [Float]
grey1 = [(50/255), (50/255), (50/255), 1]
grey2 :: [Float]
grey2 = [(75/255), (75/255), (75/255), 1]
grey3 :: [Float]
grey3 = [(88/255), (88/255), (88/255), 1]
grey4 :: [Float]
grey4 = [(100/255), (100/255), (100/255), 1]
grey5 :: [Float]
grey5 = [(125/255), (125/255), (125/255), 1]
grey6 :: [Float]
grey6 = [(150/255), (150/255), (150/255), 1]
grey7 :: [Float]
grey7 = [(175/255), (175/255), (175/255), 1]
grey8 :: [Float]
grey8 = [(200/255), (200/255), (200/255), 1]
grey9 :: [Float]
grey9 = [(225/255), (225/255), (225/255), 1]
grey10 :: [Float]
grey10 = [(250/255), (250/255), (250/255), 1]

--black
black1 :: [Float]
black1 = [0, 0, 0, 1]

--white
white1 :: [Float]
white1 = [1, 1, 1, 1]

theColor :: [Float] -> Color
theColor col = makeColor (col !! 0) (col !! 1) (col !! 2) (col !! 3)

changeColor :: Float -> [Float] -> [Float] -> Color
changeColor seconds col1 col2|seconds <= 2 = (makeColor (a seconds col1 col2) (b seconds col1 col2) (c seconds col1 col2) (d seconds col1 col2))
                             |otherwise    = (makeColor (col2 !! 0) (col2 !! 1) (col2 !! 2) (col2 !! 3))
                                where a :: Float -> [Float] -> [Float] -> Float
                                      a i x1 x2 = (x1 !! 0) + i*((x2 !! 0)-(x1 !! 0))/2
                                      b :: Float -> [Float] -> [Float] -> Float
                                      b i x1 x2 = (x1 !! 1) + i*((x2 !! 1)-(x1 !! 1))/2
                                      c :: Float -> [Float] -> [Float] -> Float
                                      c i x1 x2 = (x1 !! 2) + i*((x2 !! 2)-(x1 !! 2))/2
                                      d :: Float -> [Float] -> [Float] -> Float
                                      d i x1 x2 = (x1 !! 3) + i*((x2 !! 3)-(x1 !! 3))/2

