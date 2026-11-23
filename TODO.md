.stack-work does it have to be in this directory? i don't want it in dropbox

almost all functions return IO. there is too many lifts with pure in functions. that's not good practice is it? 
while i know there is a lot of interaction, we are being way too liberal with it. 
a good programm should be a "sensible" combination of pure islands, remember? simon peython jones and eric meijer said that i think.. we stick by it. 

also too many ifs. or ifs at all. i prefer pattern matching

infer and derive instances when possible. for example with json parsers. don't implement manually unless required.

short functions that are self contained.

i need a deployment makefile and a documentation that handles transition from the python version once done. 
but testing can be done locally here. and should be done!

error handling should really just concern the IO monad an states coming back from asterisk abnormally. the app itself is run in a controlled environment and not left for a user. so are the library functions. that means there is no reason for second guessing parameter values for everything deterministic. that should go without saying.

in general i like the code a lot more than the python version. much cleaner. but make sure to use the IO monad sparingly and also don't overdo Left/Right
