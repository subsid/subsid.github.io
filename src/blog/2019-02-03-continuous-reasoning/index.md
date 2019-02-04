---
layout: layouts/blog.liquid
title: "Continuous Reasoning: Scaling the impact of formal methods"
tags: [post, paper, software analysis]
permalink: blog/continuous-reasoning/
---

*Peter W. O’Hearn. 2018. Continuous Reasoning: Scaling the impact of formal methods. In LICS ’18: 33rd Annual ACM/IEEE Symposium on Logic in Computer Science, July 9–12, 2018, Oxford, United Kingdom. ACM, New York, NY, USA, 13 pages. https://doi.org/10.1145/ 3209108.3209109*

This paper talks about a [static program analysis](https://en.wikipedia.org/wiki/Static_program_analysis) tool called [Infer](https://github.com/facebook/infer) and its impact at facebook. *Infer* is based on a program analysis method called continuous reasoning.

> This paper describes work in continuous reasoning, where formal reasoning about a (changing) codebase is done in a fashion which mirrors the iterative, continuous model of software development that is increasingly practiced in industry

Given the prevalence of CI/CD pipelines and code review processes, the author suggests that continuous reasoning will allow formal analysis to scale to large codebases if it is integrated into the programmer's workflow. Infer is a static analysis tool for analyzing C, C++, Java, and Object-C code. Its written in OCaml. Since it runs on code diffs (rather than rebuilding the entire codebase), it is quite fast - order of low tens of minutes. This makes it a compelling tool for developers, as they get feedback during code reviews. (vs having a bug board for these errors)

One of the main reasons why Infer is fast is due to *Automatic Composability*.

> The technical feature which enables Infer’s diff-time deployment is compositionality. The idea of compositionality comes from language semantics: a semantics is compositional if the meaning of a complex phrase is defined in terms of the meanings of its parts and a means of combining them.

This idea is transferred to software analysis and is known as *Compositional Analysis*. By definition, compositional analysis does not rely on the whole program.

> Compositional Analysis: an automatic program analysis is compositional if the analysis result of a composite program is defined in terms of the analysis results of its parts and a means of combining them.

Why is it better to do this, as opposed to running a sophisticated verification tool independent of development? **ROFL ((Report Only Failure List)) Episode** At Facebook, an earlier version of Infer was deployed as a batch process running once every night or so. The tool reported a bunch of errors and these were then manually assigned to appropriate developers. As it turned out, none of these issues were prioritized by the devs. This is because of 2 main reasons:

* *Mental effort of context switch* It is hard for programmers to context switch to some old commit and work on it.

* *Relevance* Assigning an issue to the right person is a non-trivial task.

Both of these reasons are addressed by a diff based analysis approach. The Infer engine acts as a bot reviewer that gives the developer meaningful comments during the review process. The Impact of this system was enormous at facebook - tens of thousands of bugs reported by Infer were fixed.

A contrasting analysis tool, that relies on formal verification is s2n by Amazon. S2n does a full-fledged formal verification but does not scale to large codebases.

<img class="ui centered large image" src="/img/blog/s2n-vs-infer.png">

## Reporting

As mentioned before with ROFL, what and when to report is important. The following are some reporting possibilities:

> Lean reporting of only new errors only on changed files is Infer’s default at diff time. It is a low friction deployment: it avoids reporting pre-existing issues to an author of a diff, which typically will not be relevant to the diff. It supports the first axiom of industrial static analysis: Don’t spam the developer.

> Bulky reporting can, when run on a large legacy codebase, result in numerous pre-existing issues being reported. Sometimes these can be overwhelming, and irrelevant to the diff author, so care is needed in this reporting mode. (With Infer, we are experimenting with it for certain bug types and certain projects.)

> Cautious fits well with periodic global analyzer runs on an entire codebase, as opposed to at diff time. It has been used by Coverity, and versions of it are used for both static and dynamic analysis at Facebook.

> Clean is used for deployments that seek to keep a codebase entirely free of certain issues. The Amazon s2n effort uses this deployment, Infer has used it with the source code of the Buck build system, and it is commonly used with type systems.

Though *automatic composability* is hard, it may be worth spending time on improving continuous analysis, due to its scalability and impact.

