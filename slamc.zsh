ghc -dynamic main.hs Lexer.hs Parser.hs -o slamc.out

rm *.hi
rm *.o

./slamc.out example/example.slm
