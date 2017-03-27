# UH MFC Circle

This repository collects resources and information related to the 
**UH circle** of folks interested in the **Mathematical Foundations of Computing** (MFC).

## Topics

Here is a (tentative) list of some topics of interest to our group. 

1. lambda calculus
2. category theory
3. coalgebra and coinduction
4. intuitionistic type theory
5. functional programming and dependent types
6. automated theorem proving in Coq and/or Agda

The numbers of these topics are meant to be *immutable* (so don't change
them!); they are used below to indicate interests of members of the group.

----------------------------------------------------------------------------

## Members and Interests


|Name | Dept | Email | Interests|
|----- |----------| ------| -----|
|[Anthony Christe](http://www2.hawaii.edu/~achriste/) | ICS | [anthony.christe at gmail](mailto:anthony.christe@gmail.com) |5, 1, 2, 4, 3, 6|
|[Kyle Berney](http://www2.hawaii.edu/~berneyk/index.html) | ICS | [berneyk at hawaii](mailto:berneyk@hawaii.edu) | |
|[Edo Biagioni](http://www2.hawaii.edu/~esb/) | ICS | [esb at ics](mailto:esb@ics.hawaii.edu) | 2 |
|[Kelly Blumenthal](http://kelblu.weebly.com/) | IFA | [blumenthal.kelly at gmail](mailto:blumenthal.kelly@gmail) | 5, 1, 2, 4| 
|[Jason Castiglione](http://www.asecolab.org/people/jason-castiglione/)|ICS|[jcastig at hawaii](mailto:jcastig@hawaii.edu) | 3, 2, 5, 1|
|[William DeMeo](http://williamdemeo.org) | Math | [williamdemeo at gmail](mailto:williamdemeo@gmail.com) | 3, 1, 4, 5, 6, 2|
|Gabriel Dima | SAI | [gdima at hawaii](mailto:gdima@hawaii.edu) | 5, 6, 2, 1, 4, 3 |
|[Jake Fennick](https://math.hawaii.edu/wordpress/people/jfennick/)| Math| [jfennick at math](mailto:jfennickat@math.hawaii.edu) | 5, 6, 3, 4, 2, 1|
|[Patrick Karjala](https://patrickakarjala.wordpress.com/) | COE | [pkarjala at hawaii](mailto:pkarjala@hawaii.edu) | 5, 1 |
|[Bj&oslash;rn Kjos-Hanssen](https://math.hawaii.edu/wordpress/bjoern/) | Math | [bjoernkjoshanssen at gmail](mailto:bjoernkjoshanssen@gmail.com) | 3 |
|Sergey N| ICS | [allwitchesdance at gmail](mailto:allwitchesdance@gmail.com)| |
|[Hyeyoung Shin](http://hyeyoungshin.org) | Math/ICS | [hyeyoungshinw at gmail](mailto:hyeyoungshinw@gmail.com) | 4, 5, 3, 2, 1, 6|
|Michael Sommer | ICS | [mpsommer at hawaii](mailto:mpsommer@hawaii.edu) | 2, 5, 3, 4, 1, 6 |
|[Austin Tasato](http://vizualize.me/atasato) |ICS | [atasato at hawaii](mailto:atasato@hawaii.edu)| 2, 4, 5, 1, 3, 6 |
|Muzamil Mahgoub Yahia | ICS | [muzamil at hawaii](mailto:muzamil@hawaii.edu) | 2, 3, 4, 1, 5, 6|
|Troy Wooton | ICS |[gcowootont at gmail](mailto:gcowootont@gmail.com)| 5, 1, 2, 4, 6, 3|
|[Jack Yoon](https://math.hawaii.edu/wordpress/people/yoon/) | Math | [yoon at math](mailto:yoon@math.hawaii.edu)  | 6, 1, 4, 5, 3, 2 |


---------------------------------------------------------------------------

## Topic Popularity Index

To decide what topics to cover and to develop a plan for covering them, we used
the table above to derive a simple popularity index for the topics.  For each
topic number n, let ni denote the number of times n appears in the i-th position
in the team members' ordered lists of topics.  Then the popularity index is
computed as follows:

    P(n) = 6*n1 + 5*n2 + 4*n3 + 3*n4 + 2*n5 + 1*n6
	
The logic behind this ranking should be fairly obvious.  

The table below sorts the topics according to this popularity index.

| rank | description | pop index |
| --------- | ----------- | --------- |
| (A) | functional programming and dependent types | 48 |
| (B) | category theory | 42 |
| (C) | coalgebra and coinduction | 40 |
| (D) | lambda calculus | 39|
| (E) | intuitionistic type theory | 32 | 
| (F) | automated theorem proving in Coq and/or Agda | 22 |

------------------------------------------

## Tentative Plan for Spring Break, April, May

### Update: Jan 31, 2017

We hope to organize some lectures on **(A) functional programming and
dependent types** in **April** and, depending on how that goes, **(B) category
theory** and **(C) coalgebra** in **May**.  

We expect that students with little or no exposure to theoretical computer
science may feel lost if we simply dive right into functional
programming with dependent types.
Therefore, we will probably try to organize a **Spring Break Boot Camp
(March 27--31)** consisting of crash courses in some subset of the three 
other topics, (D) lambda calculus, (E) type theory, and (F) Coq/Agda. 
This will help the students become familiar with some basic principles that will
come up often when we cover topic (A).  Hopefully the boot camp experience 
will also help to motivate our study of topics (B) and (C).

More specifics will be posted here by late February.

-----------------------------------------------------------------


## Miscellaneous Info

### History

The email that kicked this off.

---------- Forwarded message ----------  
From: William DeMeo  
Date: Tue, Jan 24, 2017 at 8:28 PM  
Subject: Mathematical Foundations of Computing Science -- reading group or short course

Hello Grad Students,

I'd like to find out whether there are students at UH who would be
interested in a non-credit short course, or series of tutorials,
during the month of April, and possibly May, on some topics related
to the mathematical foundations of computing science.

Depending on your responses to this email, I could imagine running an
informal seminar or colloquium once per week, or if there is
significant interest, perhaps something a little more formal, like an
actual short course.

Here is a list of topics that I have in mind.  We could cover one,
possibly two, of these depending on student interest:

1. lambda calculus
2. category theory
3. coalgebra and coinduction
4. intuitionistic type theory
5. functional programming and dependent types
6. automated theorem proving in Coq and/or Agda

Disclaimer: I am not an expert in any of these areas, but I have had a
fair amount of exposure to all of them.  One of my motivations for
proposing this course is so that I can learn more about these topics
myself!

Please reply to this email if you are interested and tell me your
name, department, program, year, and, *most importantly*, put the list
of topics above in order from most interesting to least interesting.
For example, if you are very eager to learn about "5. functional
programming and dependent types," somewhat interested in "1. the
lambda calculus," and mildly interested in "2. category theory," then
you should write "Topic Preferences: 5, 1, 2."  Also, feel free to
suggest alternative related topics, if you wish.

Any other comments, criticisms, or suggestions are also welcomed.

Sincerely,

William
