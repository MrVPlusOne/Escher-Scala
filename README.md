# Escher-Scala
Example-Driven Recursive Program Synthesis
<hr>

This repository contains two algorithm: **TypedEscher** and **AscendRec**

**TypedEscher** is a Scala implementation of the algorithm described in this paper [Recursive Program Synthesis(CAV'13)](https://www.microsoft.com/en-us/research/publication/recursive-program-synthesis/), with the addition of a polymorphic static type system and other type-related optimizations to improve searching efficiency.

**AscendRec** is a new algorithm based on *TypedEscher*, but unlike *TypedEscher*, which requires the user to provide additional input-output examples during synthesis, *AscendRec* dose not need any additional examples to finish its work.

<br>

**Currently working in progress**

### Results (TypedEscher):

 <img src="documentation/summery.png" width="600px" alt="summery"/>

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

see also [result.txt](documentation/txt_results/result.txt)

### Results (AscendRec)

 <img src="documentation/summery_AscendRec.png" width="500px" alt="summery"/>

 see also [result_AscendRec.txt](documentation/txt_results/result_AscendRec.txt)