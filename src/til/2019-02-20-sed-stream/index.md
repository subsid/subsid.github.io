---
layout: layouts/blog.liquid
title: "Sed to rename files"
tags: [TIL, linux hacks]
permalink: til/sed-to-rename-files/
---

[Sed](https://www.gnu.org/software/sed/manual/sed.html) stands for "Stream editor". Here is a nice way to rename files with regex using sed. I was running a user study today, and mistyped the file prefix. This created 100s of files with the wrong name. My initial thought was to use a script to fix it, but then decided to lookup sed. Here is how I did it:

```sh
$ touch fooops_1.txt fooops_2.txt fooops_3.txt
$ ls
fooops_1.txt fooops_2.txt fooops_3.txt
```

Let's say our goal was to type "foobar" as the prefix. Easy to rename with sed!

```sh
$ ls | sed 's/foo\(ops\)\(.*\)/mv & foo_bar\2/'
mv fooops_1.txt foo_bar_1.txt
mv fooops_2.txt foo_bar_2.txt
mv fooops_3.txt foo_bar_3.txt
```

The key with sed is, it streams its output to stdout. Hence we can pipe it to anything!
Here, we make it output the `linux move commmand` and pipe it to `sh` to execute the command. Always good to verify command before executing it.

```sh
$ ls | sed 's/foo\(ops\)\(.*\)/mv & foo_bar\3/' | sh
$ ls
foo_bar_1.txt foo_bar_2.txt foo_bar_3.txt
```
