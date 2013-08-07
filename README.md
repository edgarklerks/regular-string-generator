Regular string generator
========================

This is a library, which generate from simple regular expressions their matching strings. It uses parsec for parsing the regular expression. I found that instead of a monad, the arrow interface was extremely useful for creating the generator. I made a special arrow (RandomArrow), which supply the random values.

The library supports the following symbols:
* Symbol 

Meaning

* Characters 

Matches a character

* (regex)\* 

Matches zero or more of the previous expression

* (regex)+ 

Matches one or more of the previous expression

* (regex)? 

Matches zero or one of the previous expression 

* (regex){n,} 

Matches n or more of the previous expression 

* (regex){,n} 

Matches between zero and n of the previous expression

* (regex){n,p} 

Matches between n and p of the previous expression

* [r1-r2p1-p2q1-..] 

Match any character between r1 and r2 or between p1 and p2 or between q1 ...

* (regex) 

group this expression 


And example expression would be:

    (http|https)://sys.AAA.org/[a-zA-Z0-9]{1,4}/[a-z][A-Z]{1,9}
