---
layout: layouts/blog.liquid
title: "Virtualenv Relinking python"
tags: [TIL, python]
permalink: til/virtualenv-relinking-python/
---

I had to update my system python today for some reason. Unfortunately, all my virtualenvs were pointing to the older system python, hence they all started failing. I had to delete the existing links and create new links.

Here is a [gist](https://gist.github.com/tevino/1a557a0c200d61d4e4fb) by [tevino](https://gist.github.com/tevino) that you can use.

Basic idea:

```bash
cd .virtualenv/{ENV}
# Find all links inside env
find . -type l
# After verifying its what you want
find . -type l -delete
# Recreate links. (Give python path if necessary)
mkvirtualenv {ENV}
```
