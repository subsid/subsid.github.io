---
layout: layouts/blog.liquid
title: Physics of flocking
tags: [post, javascript, threejs, graphics]
permalink: blog/flocking/
---

As a part my of [physics based modelling](http://courses.cs.tamu.edu/keyser/viza659/Syllabus.htm) course, I implemented a [flocking simulation](https://subsid.github.io/flocking/dist/) using [threejs](https://threejs.org/).

Flocking (or Swarming) is a nice example of something known as *emergent behavior*.
[From wikipedia](https://en.wikipedia.org/wiki/Emergence):

> Emergence is a phenomenon whereby larger entities arise through interactions among smaller or simpler entities such that the larger entities exhibit properties the smaller/simpler entities do not exhibit.

This behavior is common among various animal groups such as birds, ants, bees, fishes. Here is a beautiful video of starlings flocking around.

<div class="iframe_container">
  <iframe width="560" height="315" src="https://www.youtube.com/embed/V4f_1_r80RY?rel=0" frameborder="0" allowfullscreen='allowfullscreen'> </iframe>
</div>

The idea was popularized in the field of graphics by [Craig Reynolds](https://en.wikipedia.org/wiki/Craig_Reynolds_(computer_graphics)), when he created the famous [boids](https://en.wikipedia.org/wiki/Boids) artificial life simulation in 1986.

At its core, each Boid (Bird-oid object!) follows three simple rules:

* **seperation** Avoid crowding local buddies.
* **alignment** Direction of velocity should be along the average direction of local buddies.
* **cohesion** Stay centered between local buddies.

And that's it! This leads to some cool emergent behavior. This can be extended in many ways, by adding some obstacle avoidance, common goal and other effects. Something I find really cool is that we can start each boid at some random initial velocity, and see them slowly come together into a nice flock.

The source code is available [here](https://github.com/subsid/flocking), in case someone wants to play with it. The implementation is based on [this pen by Ellie](https://codepen.io/coaster/pen/QpqVjP).

