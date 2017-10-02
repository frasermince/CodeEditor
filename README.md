# TextEditor

Hello! I’m going to walk you through how I implemented this program.

I used a state monad transformer to hold the history. I originally had it hold a list of strings.
However I eventually had to change this because it was not in anyway optimized. I tried both Text and ByteString and 
discovered ByteString is actually faster when only ASCII characters are used. Since we're only using basic characters
for input I decided to use ByteString for input and output. 

I was originally parsing the commands with parsec but changed to megaparsec because it is a similar interface but faster.
After running it through criterion I discovered I managed to shave about half a second off with a million input 
commands after switching to megaparsec.

I then started to realize that a lot of the time complexity comes from `append`s as well as `index`es and `take`s on my 
text. I experimented with having the actual editor text inside the state monad be bytestring, string, and text, but I 
finally decided to store the text in a sequence. The nice thing about sequence is concatenating, indexing, and taking can 
all be done in O (log n) time. This is by far the most efficient datastructure I've found for these three operations in 
Haskell. That said I still decided to use ByteString for I/O.

The history stack is a list of sequences (that represent states in history) that contain characters. I experimented around 
with changing the data structure of the history list. However since the list is used as a stack there is little overhead. 
We only are ever concerned about the first element on the stack so all operations on it are O(1). 

I also was originally just mapping over all the input and then applying the commands to the text. I decided to refactor to 
use the pipes library. This gives us streaming and means as soon as we get input we start consuming it. This also makes 
the code very clean because it is modeled as a single pipeline.

My only frustration with this code is I didn't realize that megaparsec is not on hackerrank. However migrating to it did 
save enough time that I consider it worth it. To make things even faster and make it work with hackerrank I would need to 
change to attoparsec. However I think this at least demonstrates my abilities fairly well. This has been a great 
opportunity to learn about pipes, criterion, text types, and generally optimizing a haskell program.

To run it you should just be able to do:
`stack build` and then `stack exec textEditor-exe`.
Also feel free to check out my test suite and benchmarks with `stack test` and `stack bench`.
Haddock documentation can be generated with `stack haddock`.


