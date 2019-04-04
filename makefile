default: Main
	./Main script

Main: Main.hs Parse.hs Transform.hs Line.hs Solids.hs Screen.hs DrawMats.hs
	ghc -dynamic -O -O2 Main.hs

build: Main

clean:
	rm *.hi *.o Main *.ppm *.png .tempimg.ppm

imgclean:
	rm *.ppm *.png .tempimg.ppm

run:
	./Main script
