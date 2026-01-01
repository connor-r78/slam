ghc -dynamic main.hs lexer.hs parser.hs -o slamc.out

rm *.hi
rm *.o

./slamc.out example/example.slm
