---
layout: layouts/blog.liquid
title: "Slow Org-mode? use narrow subtree"
tags: [TIL, emacs, org-mode]
permalink: til/org-narrow-subtree/
---

My org-mode buffer has been getting slower over time. [stackoverflow](https://stackoverflow.com/questions/40793325/emacs-diagnosis-org-mode-unbearably-slow-and-often-stalls/40794538) has some good suggestions to speed it up. The one that worked for me was to open all the nodes. This made it superfast and not laggy. You can do this by adding this line to the top of your org file.

```org
#+STARTUP: showeverything
```

However, working on an org file with all nodes expanded can be annoying. You can use `org-narrow-to-subtree` command to just show the node you are working on. (Default mapping: `C-x n s`) Expand back to wide view using `widen` (`C-x n w`).

