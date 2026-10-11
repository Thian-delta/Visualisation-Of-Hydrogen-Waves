1. Hydrogen Wave Function

A program that visualizes the cross-section of the probability density plot of an electron in a hydrogen atom up until n = 3. (For further details, see: https://chem.libretexts.org/Courses/University_of_California_Davis/Chem_107B%3A_Physical_Chemistry_for_Life_Scientists/Chapters/4%3A_Quantum_Theory/4.10%3A_The_Schr%C3%B6dinger_Wave_Equation_for_the_Hydrogen_Atom)
The density of particles (represented by circles) in a given area correlates with the probability of finding an electron there. The colors displayed are purely for aesthetic purposes and do not carry any significance related to the wave function itself.

Chemistry is a subject I focused on extensively in high school, and it has become a part of me. Therefore, through this project, I want to combine my passion for chemistry with my growing interest in informatics.

2. Reference

There are some references that I use to make this program:
https://www.youtube.com/watch?v=40ftYOfsEDU
https://github.com/MacSlow/haskell-gimmicks/blob/master/random-points-on-sphere.hs

3. Cabal

To run this project, you will need to have the latest version of cabal installed on your machine.
See: https://www.haskell.org/cabal/

4. How to run?

Unzip the folder `haskell project`.
Once cabal is installed, you can run this program by typing the following commands in your terminal:
$ cd <directory of the file>
$ cabal run
Ensure that you replace <directory of the file> with the actual path to the project directory.

5. Problem with GLUT

If you encounter the error `fpcompetition: user error (unknown GLUT entry glutInit)` after running the program, here are a few steps you can do:
Windows:
- Download the MinGW version of FreeGLUT from http://www.transmissionzero.co.uk/software/freeglut-devel/
- Extract the ZIP file and navigate to <freeglut-MinGW-3.0.0-1.mp.zip\freeglut\bin\x64\>
- Copy the file freeglut.dll to <C:\Windows\System32\>
- Rename the copied file to glut32.dll
OS X:
- Open your terminal and run the following command to install GLUT via Cabal: 
  $ cabal install GLUT --ghc-options="-optl-Wl,-framework,GLUT" --reinstall --jobs=1
Ubuntu:
- Open your terminal and run the following command to install FreeGLUT:
  $ sudo apt-get install freeglut3

6. Sample video

I have included an MP4 video in the `sample video` folder that demonstrates the output of my program. However, since I couldn't find a way to directly export or render the output as an MP4, I had to resort to screen recording and cropping the video. As a result, the video quality may not be optimal. For the best results, I recommend running the program on your own device.

7. Additional Files

The `additional files` folder contains supporting materials that are not directly related to the program itself. These are just some helper files I used during the development of the project.

8. Room for improvements

The main one is the visualization of the `angular wave function`. Hydrogen wave functions consist of two parts: radial and angular wave function. The radial wave function depends solely on the radial distance, r, from the nucleus, while the angular wave function describes the directionality and defines the appearance of the orbital. At present, the angular wave function only assigns probabilities of 0 or 1, with no intermediate values, limiting the accuracy of the visualization.

9. License

This file is not licensed (all rights reserved). However, I kindly request that you contact me prior to making any modifications to the code.
Another thing is the `inefficiency` of the code. While the program runs fairly smoothly, the code is quite lengthy and repetitive. It would be beneficial to refine the code, particularly in the definition of animations for each frame and each color. Optimizing the code structure could improve readability while also potentially enhancing performance.

9. License
This file is not licensed (all rights reserved). However, I kindly request that you contact me prior to making any modifications to the code.
 
