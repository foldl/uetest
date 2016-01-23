UETEST
======

A system for testing UE (user equipment) with UE tester (testing instrument), built on Erlang.

It controls testers and UEs according to case scripts. It can also control Monsoon Solution's
Power Monitor to evaluate UE's power consumption.

Features
---------------

* Robust
* Easy for repeating tests
* Easy to design new cases
* Graphs in test reports
* Easy to deploy
* Lots of predefined cases (ICS/Measurement/IRAT)
* Power consumption measurement
* Support Dual-SIM (AT can be reused or not) 

UE
---------------

Multi-mode (2G/3G/4G) mobile phones (evaluation boards).

This system controls UE through AT. By implementing a dedicated Erlang port, log
capturing during tests is also possible. (See uelog\_control.erl)

Android ADB is used occasionally. 

UE testers
---------------

Supported testers: Datang 3110, Anristru 8820.

It's easy to add new types. (See dt\_3110\_helper.erl)

Testers can be controlled through GPIB/VXI/HiSlip.

PS
---------------

If someone do want to have a try, feel free to reach me for support.
