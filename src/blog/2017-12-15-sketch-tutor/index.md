---
layout: layouts/blog.liquid
title: Sketch Tutor - Game based learning
tags: [post, javascript, react, sketch recognition]
permalink: blog/sketch-tutor/
---

Sketch recognition is the automated recognition of hand drawn diagrams. In general, sketch recognition techniques can be classified into three types:

- **Appearance based**
This comes more from the field of computer vision, but is not very useful for varying shapes. It does not take temporal data into account.

- **Gesture based** Most useful for forensic methods, but requires user specific training. (Every individual has their own quirks when sketching!)

- **Geometric based** Models are built based on Geometric constraints. Requires neater sketches, but very flexible.

The field is quite fascinating, and methods here can be extended into areas of activity recognition and eye tracking. [Dr. Hammond's SRL Lab](http://srl.tamu.edu) has some interesting research in the area.

Anyways, *Sketch Tutor* is a sketch based game for learning new symbols. Similar to the old game typing-tutor, the idea is to gamify learning a new language, numerals or set of symbos in general. The prototype version of the game supports learning 1-10 numbers in chinese. The scoring is based on a combination of accuracy and number of attempts.

The generalized recognition algorithm is based on the [$P algorithm](http://faculty.washington.edu/wobbrock/pubs/icmi-12.pdf) from [UWash](https://depts.washington.edu/madlab/proj/dollar/pdollar.html). While the current version of the game supports chinese characters from 1-10, the system can be easily extended to have a training phase, where the generalized learning algorithm can be trained on a new set of symbols.

Here is the [demo](https://subsid.github.io/sketch-tutor/) and [source code](https://github.com/subsid/sketch-tutor). Do let me know what you think.

