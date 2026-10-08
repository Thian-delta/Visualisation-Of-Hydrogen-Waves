module HydrogenWaveFunction
    (subLevel1s, subLevel2s, subLevel3s, subLevel2px, subLevel2py, subLevel3px, subLevel3py, subLevel3dz2, subLevel3dzx, subLevel3dxy) where

import System.IO ()
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import System.Random

--to convert cartesian to polar
polarToCartesian :: Float -> Float -> Point
polarToCartesian radius theta = (x, y)
    where thetaRad = degToRad theta
          x = radius * cos thetaRad
          y = radius * sin thetaRad

subLevel1s :: [Point]
subLevel1s = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [randomThetas1 x|x <- radiuss1])
    where psi1 :: Float -> Float
          psi1 x = 1/sqrt pi*(1/0.529)**(3/2)*exp (- (x / 0.529)) -- 0.529 is Bohr radius in Angstrom
          pdf1 :: Float -> Float
          pdf1 x = x**2*psi1 x**2
          particleNum1 :: Float -> Int
          particleNum1 x = floor (totalParticle1*pdf1 x/4)
          totalParticle1 = 10000
          radiuss1 = map (/50) [1..500]
          randomNum1 :: Float -> Int
          randomNum1 radius = the [y|(x,y) <- zip radiuss1 (take 500 $ randomRs (1 :: Int, 10000) $ mkStdGen 23850), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          randomThetas1 :: Float -> [(Float, Float)]
          randomThetas1 radius = zip (repeat radius) thetas1
            where thetas1 = take (particleNum1 radius) $ randomRs (0.0::Float, 360.0) $ mkStdGen (randomNum1 radius)

subLevel2s :: [Point]
subLevel2s = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [randomThetas2 x|x <- radiuss2])
    where psi2 :: Float -> Float
          psi2 x = 1/(4*sqrt (2*pi))*(1/0.529)**(3/2)*(2-x/0.529)*exp (- (x / (2 * 0.529))) -- 0.529 is Bohr radius in Angstrom
          pdf2 :: Float -> Float
          pdf2 x = x**2*psi2 x**2
          particleNum2 :: Float -> Int
          particleNum2 x = floor (totalParticle2*pdf2 x/4)
          totalParticle2 = 10000
          radiuss2 = map (/50) [1..500]
          randomNum2 :: Float -> Int
          randomNum2 radius = the [y|(x,y) <- zip radiuss2 (take 500 $ randomRs (1 :: Int, 10000) $ mkStdGen 23850), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          randomThetas2 :: Float -> [(Float, Float)]
          randomThetas2 radius = zip (repeat radius) thetas2
            where thetas2 = take (particleNum2 radius) $ randomRs (0.0::Float, 360.0) $ mkStdGen (randomNum2 radius)

subLevel3s :: [Point]
subLevel3s = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [randomThetas3 x|x <- radiuss3])
    where psi3 :: Float -> Float
          psi3 x = 1/(81*sqrt (3*pi))*(1/0.529)**(3/2)*(27 - 18*(x/0.529) + 2*(x/0.529)**2)*exp (- (x / (3 * 0.529))) -- 0.529 is Bohr radius in Angstrom
          pdf3 :: Float -> Float
          pdf3 x = x**2*psi3 x**2
          particleNum3 :: Float -> Int
          particleNum3 x = floor (totalParticle3*pdf3 x/4)
          totalParticle3 = 10000
          radiuss3 = map (/50) [1..500]
          randomNum3 :: Float -> Int
          randomNum3 radius = the [y|(x,y) <- zip radiuss3 (take 500 $ randomRs (1 :: Int, 10000) $ mkStdGen 23850), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          randomThetas3 :: Float -> [(Float, Float)]
          randomThetas3 radius = zip (repeat radius) thetas3
            where thetas3 = take (particleNum3 radius) $ randomRs (0.0::Float, 360.0) $ mkStdGen (randomNum3 radius)

subLevel2px :: [Point]
subLevel2px = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [repeatedThetas4 x|x <- radiuss4])
    where psi4 :: Float -> Float
          psi4 x = 1/(4*sqrt (2*pi))*(1/0.529)**(3/2)*(x/0.529)*exp (- (x / (2 * 0.529))) -- 0.529 is Bohr radius in Angstrom
          pdf4 :: Float -> Float
          pdf4 x = x**2*psi4 x**2
          particleNum4 :: Float -> Int
          particleNum4 x = floor (totalParticle4*pdf4 x/8)
          totalParticle4 = 10000
          radiuss4 = map (/33) [1..330]
          randomNum4 :: Float -> Int
          randomNum4 radius = the [y|(x,y) <- zip radiuss4 (take 500 $ randomRs (1 :: Int, 10000) $ mkStdGen 23094), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          thetas4 :: Float -> [Float]
          thetas4 radius = take (particleNum4 radius) [x|x <- take 1000 $ randomRs (0.0 :: Float, 360.0) $ mkStdGen (randomNum4 radius), ((10/2)**(1/2)*cos (degToRad x))**2 >= radius]
          repeatedThetas4 :: Float -> [(Float, Float)]
          repeatedThetas4 radius = zip (repeat radius) (thetas4 radius)

subLevel2py :: [Point]
subLevel2py = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [repeatedThetas5 x|x <- radiuss5])
    where psi5 :: Float -> Float
          psi5 x = 1/(4*sqrt (2*pi))*(1/0.529)**(3/2)*(x/0.529)*exp (- (x / (2 * 0.529))) -- 0.529 is Bohr radius in Angstrom
          pdf5 :: Float -> Float
          pdf5 x = x**2*psi5 x**2
          particleNum5 :: Float -> Int
          particleNum5 x = floor (totalParticle5*pdf5 x/8)
          totalParticle5 = 10000
          radiuss5 = map (/33) [1..330]
          randomNum5 :: Float -> Int
          randomNum5 radius = the [y|(x,y) <- zip radiuss5 (take 500 $ randomRs (1 :: Int, 10000) $ mkStdGen 67426), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          thetas5 :: Float -> [Float]
          thetas5 radius = take (particleNum5 radius) [x|x <- take 1000 $ randomRs (0.0 :: Float, 360.0) $ mkStdGen (randomNum5 radius), ((10/2)**(1/2)*sin (degToRad x))**2 >= radius]
          repeatedThetas5 :: Float -> [(Float, Float)]
          repeatedThetas5 radius = zip (repeat radius) (thetas5 radius)

subLevel3px :: [Point]
subLevel3px = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [repeatedThetas5 x|x <- radiuss5])
    where psi5 :: Float -> Float
          psi5 x = 1/(81*sqrt pi)*(1/0.529)**(3/2)*(x/0.529)*(6 - x/0.529)*exp (- (x / (3 * 0.529))) -- 0.529 is Bohr radius in Angstrom
          pdf5 :: Float -> Float
          pdf5 x = x**2*psi5 x**2
          particleNum5 :: Float -> Int
          particleNum5 x = floor (totalParticle5*pdf5 x/3)
          totalParticle5 = 10000
          radiuss5 = map (/27) [1..270]
          randomNum5 :: Float -> Int
          randomNum5 radius = the [y|(x,y) <- zip radiuss5 (take 750 $ randomRs (1 :: Int, 10000) $ mkStdGen 23094), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          thetas5 :: Float -> [Float]
          thetas5 radius = take (particleNum5 radius) [x|x <- take 1000 $ randomRs (0.0 :: Float, 360.0) $ mkStdGen (randomNum5 radius), (10**(1/2)*cos (degToRad x))**2 >= radius]
          repeatedThetas5 :: Float -> [(Float, Float)]
          repeatedThetas5 radius = zip (repeat radius) (thetas5 radius)

subLevel3py :: [Point]
subLevel3py = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [repeatedThetas6 x|x <- radiuss6])
    where psi6 :: Float -> Float
          psi6 x = 1/(81*sqrt pi)*(1/0.529)**(3/2)*(x/0.529)*(6 - x/0.529)*exp (- (x / (3 * 0.529))) -- 0.529 is Bohr radius in Angstrom
          pdf6 :: Float -> Float
          pdf6 x = x**2*psi6 x**2
          particleNum6 :: Float -> Int
          particleNum6 x = floor (totalParticle6*pdf6 x/3)
          totalParticle6 = 10000
          radiuss6 = map (/27) [1..270]
          randomNum6 :: Float -> Int
          randomNum6 radius = the [y|(x,y) <- zip radiuss6 (take 750 $ randomRs (1 :: Int, 10000) $ mkStdGen 23094), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          thetas6 :: Float -> [Float]
          thetas6 radius = take (particleNum6 radius) [x|x <- take 1000 $ randomRs (0.0 :: Float, 360.0) $ mkStdGen (randomNum6 radius), (10**(1/2)*sin (degToRad x))**2 >= radius]
          repeatedThetas6 :: Float -> [(Float, Float)]
          repeatedThetas6 radius = zip (repeat radius) (thetas6 radius)

subLevel3dz2 :: [Point]
subLevel3dz2 = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [repeatedThetas7 x|x <- radiuss7])
    where psi7 :: Float -> Float
          psi7 x = 1/(81*sqrt (6*pi))*(1/0.529)**(3/2)*(x/0.529)**2*exp (- (x / (3 * 0.529))) -- 0.529 is Bohr radius in Angstrom
          pdf7 :: Float -> Float
          pdf7 x = x**2*psi7 x**2
          particleNum7 :: Float -> Int
          particleNum7 x = floor (totalParticle7*pdf7 x/3)
          totalParticle7 = 10000
          radiuss7 = map (/30) [1..300]
          randomNum7 :: Float -> Int
          randomNum7 radius = the [y|(x,y) <- zip radiuss7 (take 750 $ randomRs (1 :: Int, 10000) $ mkStdGen 563), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          thetas7 :: Float -> [Float]
          thetas7 radius = take (particleNum7 radius) [x|x <- take 1000 $ randomRs (0.0 :: Float, 360.0) $ mkStdGen (randomNum7 radius), ((10/4)**(1/2)*(3*cos (degToRad x)**2 - 1))**2 >= radius]
          repeatedThetas7 :: Float -> [(Float, Float)]
          repeatedThetas7 radius = zip (repeat radius) (thetas7 radius)

subLevel3dzx :: [Point]
subLevel3dzx = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [repeatedThetas8 x|x <- radiuss8])
    where psi8 :: Float -> Float
          psi8 x = 1/(81*sqrt pi)*(1/0.529)**(3/2)*(x/0.529)**2*exp (- (x / (3 * 0.529))) -- 0.529 is Bohr radius in Angstrom
          pdf8 :: Float -> Float
          pdf8 x = x**2*psi8 x**2
          particleNum8 :: Float -> Int
          particleNum8 x = floor (totalParticle8*pdf8 x/12)
          totalParticle8 = 10000
          radiuss8 = map (/20) [1..200]
          randomNum8 :: Float -> Int
          randomNum8 radius = the [y|(x,y) <- zip radiuss8 (take 750 $ randomRs (1 :: Int, 10000) $ mkStdGen 563), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          thetas8 :: Float -> [Float]
          thetas8 radius = take (particleNum8 radius) [x|x <- take 1000 $ randomRs (0.0 :: Float, 360.0) $ mkStdGen (randomNum8 radius), (9*sin (degToRad x)*cos (degToRad x)**2)**2 >= radius]
          repeatedThetas8 :: Float -> [(Float, Float)]
          repeatedThetas8 radius = zip (repeat radius) (thetas8 radius)

subLevel3dxy :: [Point]
subLevel3dxy = map ((\(a, b) -> (a*30, b*30)) . uncurry polarToCartesian) (concat [repeatedThetas9 x|x <- radiuss9])
    where psi9 :: Float -> Float
          psi9 x = 1/(162*sqrt pi)*(1/0.529)**(3/2)*(x/0.529)**2*exp (- (x / (3 * 0.529))) -- 0.529 is Bohr radius in Angstrom
          pdf9 :: Float -> Float
          pdf9 x = x**2*psi9 x**2
          particleNum9 :: Float -> Int
          particleNum9 x = floor (totalParticle9*pdf9 x/3)
          totalParticle9 = 10000
          radiuss9 = map (/20) [1..200]
          randomNum9 :: Float -> Int
          randomNum9 radius = the [y|(x,y) <- zip radiuss9 (take 750 $ randomRs (1 :: Int, 10000) $ mkStdGen 563), x == radius]
            where the :: [a] -> a
                  the xs|length xs == 1 = head xs
                        |otherwise      = error "The list contain more than 1 element."
          thetas9 :: Float -> [Float]
          thetas9 radius = take (particleNum9 radius) [x|x <- take 1000 $ randomRs (0.0 :: Float, 360.0) $ mkStdGen (randomNum9 radius), (10**(1/2)*sin (degToRad x)**2*cos (2* degToRad x))**2 >= radius]
          repeatedThetas9 :: Float -> [(Float, Float)]
          repeatedThetas9 radius = zip (repeat radius) (thetas9 radius)

