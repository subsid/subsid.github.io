---
layout: layouts/blog.liquid
title: "Toward a Unified Ontology of Cloud Computing"
tags: [post, paper, cloud computing]
permalink: blog/paper-ontology-cloud-computing/
---

*[Toward a Unified Ontology of Cloud Computing](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.176.3634&rep=rep1&type=pdf) L. Youseff, M. Butrico, and D. Da Silva, 2008 Grid Computing Environments Workshop, Austin, TX, 2008, pp. 1-10.*

This paper is one of the early (relatively) works that summarizes the various components of Cloud Computing. At the time (2008), *AWS* was in the market only for a couple of years and Google cloud was just getting started. Thinking back, the classification described here is pretty much how most offerings these days are grouped.

> Cloud Computing is one contemporary technology in which the research community has recently embarked. Manifesting itself as the descendant of several other computing research areas such as Service-Oriented Architecture, distributed and grid computing, and virtualization, cloud computing inherits their advancements and limitations.

The authors keep **composability** and **layering** as the central theme to their classification. i.e. Larger components are made from smaller components (taken from [SOA](https://en.wikipedia.org/wiki/Service-oriented_architecture)) and each layer is built from lower layer components.

<img class="ui centered big image" src="/img/blog/cloud-layers.png" alt="Cloud Computing Layers" />

*Cloud Application Layer* is the most visible layer to the end users of a cloud service. Applications here are referred to as **Software as a service**, something we take for granted these days. Its probably what has led to the software startup boom. The ease of getting an application to production, with minimal maintenance, installation and other issues faced by **on-prem** deployments.

> Despite all the advantageous benefits of this model, several deployment issues hinder its wide adoption.  Specifically, the security and availability of cloud applications are two of the major issues in this model, and they are currently avoided by the use of lenient service level agreements (SLA)

We've definitely come a long way in handling this problem over the last 10 years, with better monitoring ([Prometheus](http://https://prometheus.io/), [Datadog](https://www.datadoghq.com/)), deployment ([Docker](http://https://www.docker.com/), [K8s](https://kubernetes.io/)), [CI](https://en.wikipedia.org/wiki/Continuous_integration)/[CD](https://en.wikipedia.org/wiki/Continuous_delivery) pipelines ([Jenkins, Teamcity, Travis](https://stackify.com/top-continuous-integration-tools/)).

*Cloud Software Infrastructure layer* is what is commonly referred to as [PaaS](https://en.wikipedia.org/wiki/Platform_as_a_service). This covers the various tools for application developers, that are used to build the SaaS application. [AWS-CLI](https://aws.amazon.com/cli/) in one example. This layer is where most abstractions are built for making it easier to get apps to production. Having an API for scaling, load-balancing, etc. Of course, the application developer can always bypass this layer if they want more pain :)

*Cloud Software Infrastructure Layer* can be divided into three parts.

1) Computational Resources (dubbed **Infrastructure as a service**)
[Virtualization](https://en.wikipedia.org/wiki/Virtualization) is the prime enabler for cloud infrastructure. It is what allows the provider to make money - Timesharing limited hardware for multiple users. Amazon's EC2 is a good example of such a resource.

> This was specifically enabled by two virtualization technologies: paravirtualization and hardware-assisted virtualization. Although both virtualization technologies have addressed performance isolation between virtual machines contending on common resources, performance interference between VMs sharing the same cache and TLB hierarchy cannot yet be avoided [12]. Further, the emergence of multicore machines into mainstream servers exacerbate this performance interference problem.  In turn, the lack of strict performance isolation between VMs sharing the same physical node has resulted in the inability of cloud providers to give strong guarantees for performance to their clients.  Instead, they offer them unsatisfactory SLAs in order to provide competitive pricing for the service. Such weak guarantees, unfortunately, can inject themselves up the layers of the cloud stack, and affect the SLAs of the cloud systems built above the IaaS’s SLAs.

2) Data Storage
Storage services such as S3, RDS, etc.

3) Communication
This is an interesting component, where a lot of work has gone in recently. But I think most of the work in this area has come through applications that are deployed by the developer. (Such as Kafka, Apache Flink, etc.) While there are services such as Kinesis, SNS by AWS that do some of this, I think there is scope to provide services that are application independent in this layer.

> Consequently, cloud systems are obliged to provide some communication capability that is service oriented, configurable, schedulable, predictable, and reliable. Towards this goal, the concept of Communication as a Service (CaaS) emerged to support such requirements, as well as network security, dynamic provisioning of virtual overlays for traffic isolation or dedicated bandwidth, guaranteed message delay, communication encryption, and network monitoring.


The authors also mention *Software Kernel* and *Hardware/Firmware* as the two lowest layers, but these are mostly managed by the cloud provider, rather than the consumer.

<img class="ui centered big image" src="/img/blog/cloud-layers-table.png" alt="Cloud Computing Systems" />

The origins of modern cloud computing are often forgotten. . .

> Huge coperation, like Amazon and Google generally build their computing infrastructure to support the peak demand of their business. As the average demand of the system is however, several times smaller than the peak demand [18], their computing infrastructure is usually under-utilized and the cost of its operation constitutes an additional expense. In order to offset this extra expense, they have offered utilizing this surplus computing power as a service when it is not used by their business systems.  Their offerings came at very attractive prices since they have deployed their systems at a large scale, and thus benefit from the economy-of-scale.

As mentioned by the authors, the biggest challenges in cloud computing are security and monitoring of large distributed services. (Still the case 10 years hence)

