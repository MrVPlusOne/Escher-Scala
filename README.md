# Escher-Scala
Example-Driven Recursive Program Synthesis
<hr>

This repository contains the source code of this thesis [Oracle-free Synthesis of Recursive Programs](documentation/AscendRec-en.pdf).

The codebase implements two algorithms: **TypedEscher** and **AscendRec**

**TypedEscher** is a Scala implementation of the *Escher* algorithm, described in this paper [Recursive Program Synthesis(CAV'13)](https://www.microsoft.com/en-us/research/publication/recursive-program-synthesis/), with the addition of a polymorphic static type system and other type-related optimizations to improve searching efficiency.

**AscendRec** is a new algorithm based on *TypedEscher*, but unlike *TypedEscher*, which requires the user to provide additional input-output examples during synthesis, *AscendRec* dose not need any additional examples to work.

<br>

### Results taken from the [thesis](documentation/AscendRec-en.pdf).

 <img src="documentation/TypedEscherResults.png" width="660px" alt="summery"/>

 <img src="documentation/AscendRecResults.png" width="660px" alt="summery"/>


### Some Synthesized Programs(TypedEscher):


- Duplicate each element of a list

<img src="documentation/Stutter.png" width="600px" alt="stutter"/>

- Cartesian product of two lists

<img src="documentation/cartesian.png" width="600px" alt="cartesian product"/>

- Square of naturals

<img src="documentation/squareList.png" width="600px" alt="square list"/>

- Remove adjacent duplicates in a list

<img src="documentation/compress.png" width="600px" alt="compress"/>

- Remove all duplicates in a list (synthesized without using additional components)

<img src="documentation/dedup.png" width="600px" alt="dedup"/>

See full output log in [result_TypedEscher.txt](documentation/text_results/result_TypedEscher.txt) and [result_AscendRec.txt](documentation/txt_results/result_AscendRec.txt)

