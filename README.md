This is a Micro16 simulator written in Scala.

Currently, it's in a very early stage, so don't expect too much ;)
However, it currently understands and executes all valid micro16 code (at least 
as far as I know). 

![Screen shot 1](http://i.imgur.com/tUgkd.png)

I'm taking advantage of Scala's parser-combinators to save a lot of time - the
definition of the whole language only takes about 30 lines of code. As far as I 
know, it currently matches all valid micro16 code.

This project is using sbt as a build tool. Just change into the project directory,
execute sbt and type 'run'.

Alternatively, you can just download the latest jar from github and execute it:
java -jar micro16-assembly-1.0.jar micro16-code.txt

If you want to load code from a file while running sbt, just add the file name
after the run command, like this: 
> run micro16-code.txt
The search path is relative to the root of the project dir.
If you run without arguments, the program will wait for input on stdin - either
pipe it in or write it manually.