# The Amy Reference Compiler

We provide you with a reference compiler for the Amy language that you can use to explore the expected behavior of your own compiler.

The reference compiler is [available here](https://gitlab.epfl.ch/lara/cs320/-/blob/main/labs/amyc-assembly-1.7.jar).

You can run it as follows:
  java -cp amyc-assembly-1.7.jar amyc.Main [options] [input files]

To see the list of available options do 
  java -cp amyc-assembly-1.7.jar amyc.Main --help

The easiest way to execute a program is to run it in `--interpret` mode. If you want to generate WebAssembly binary code, you should follow these steps:
  * Install `nodejs`. We have tested amyc with Node version 12, though other versions might work. See the [Node website](https://nodejs.org/en/) for installation instructions.
  * Make sure the `wat2wasm` executable is visible, i.e., it is in the system path or, for Linux or Mac, you are at the toplevel of the `amyc` directory.
  * Run `npm install deasync` in the directory you plan to run `amyc`.
  * Run `amyc` without options. You will get a few output files.
  * Run `nodejs <output file>.js`

