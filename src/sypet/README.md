# SyPet 2.0

SyPet is a program synthesis tool for Java libraries that automatically constructs programs by composing APIs. SyPet 2.0 is a clean implementation of the synthesis algorithm described in the POPL paper: 

```
Component-Based Synthesis for Complex APIs. POPL 2017.
Yu Feng, Ruben Martins, Yuepeng Wang, Isil Dillig, Thomas W. Reps. 
```

Version 2.0 of SyPet has the following additional features (some under development):
* Atom plug-in for increased usability;
* Use of program analysis to prune equivalent infeasible programs;
* Use of machine learning to guide SyPet's search;
* Webserver version that can be deployed in any remote server;
* Improved performance by changing the underlying graph representation;
* Improved performance by caching previous Soot executions;
* Improved performance by incremental construction of the SAT encoding;
* Improved performance by running multiple instances of SyPet in parallel;
* User can give hints to the synthesizer using keywords or APIs;
* Limited support for conditionals and loops.

If you want a new feature implemented into SyPet, please create an issue with your request.

More details about SyPet can be found at:
* Webpage: https://utopia-group.github.io/sypet/
* GitHub SyPet 1.0: https://github.com/utopia-group/sypet

# Acknowledgments

SyPet 2.0 is mainly developed and maintained by Ruben Martins at CMU. We would like to thank the UToPiA research group lead by Isil Dillig at UT Austin and Yu Feng at UCSB (former PhD student at UT Austin) for their collaboration in this project. We would also like to thank all contributors to this project, namely, Yuepeng Wang at UT Austin, Tom Reps at UW-Madison and Kaige Liu, Anlun Xu, Tianlei Pan and Mayank Jain at CMU.
