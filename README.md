Micro16 Simulator
=================

This is a Micro16 simulator written in Scala, using swing for GUI.

Currently, it's in an early stage, so don't expect too much ;)
Still, it currently understands and executes all valid micro16 code known to me
It also allows you to single step through code while viewing registers, current
execution position and memory, so it's quite useful to debug your programs.

Note: I don't guarantee that this is exactly the same micro16-syntax as used
in the slides of TGI class, but I do my best to ensure it is.

Screenshot
==========

![Screen shot 1](http://i.imgur.com/wP4Zm.png)

Usage
=====

Precompiled .jar
----------------

This is the easy way: just head over to the downloads section, grab the newest
.jar and execute it like this:

    java -jar micro16-sim-0.2.jar state-machine.txt


Compiling from source
---------------------

This is for those who want bleeding edge code. While I'm trying to keep the codebase
working at all times, it may still be broken. So use this at your own risk.

Clone this git repository to some directory on your PC, then type 

    sbt run state-machine.txt

to compile and run. (Note: you need sbt setup first, of course)

Implementation
==============
I'm taking advantage of Scala's parser-combinators to save a lot of time - the
definition of the whole language only takes about 35 lines of code. As far as I 
know, it currently matches all valid micro16 code.

If you find any micro16 code I haven't implemented yet, please tell so I can fix this.
Pull-request are of course always welcome.
