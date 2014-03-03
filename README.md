To-d-O-matic
============

To-d-O-Matic 0.1.0

Copyright 2014 Sak Lee <mail@saklee.net>
Some rights reserved.

Written in Chicken Scheme 4.8.0


Description
===========

To-d-O-Matic is a console-based to-do planner with the ncurses frontend and a corny name. It is designed to be lightweight, minimalistic, and intuitive, and is made for those who always keep at least one console window open, who do not trust the web-based to-do apps with slow response and excess eye-candy, and who do not have time to check their "iWhatever" devices every five minutes.

This software is under alpha stage, and most of the features are not implemented.


Changes
=======

+ 0.1.0
 - Basic, runnable ncurses skeleton with file IO.


License
=======

To-d-O-Matic is released under GNU GPL v2. Distribution, modification, and distribution of the modified material is permitted as long as the original copyright is intact and the modified source code is re-released. Please read attached LICENSE for more detail.


Install
=======

run "csc todo.scm"


Requirement
===========

- Chicken Scheme 4.8.0
- GCC 4.X (The minimum requirement not confirmed.)
- ncurses 5.9
- Chicken Scheme Extension: ncurses, directory-utils, srfi-19


Features
========

+ Implemented:
 - Auto-save and load on start and exit
 - Toggle between todo list and done list

+ Unimplemented:
 - Add, delete, and defer items
 - Append notes for the detail
 - Order and color items based on time priority
 - Categorize with custom categories
 - Auto-completion for every text entry
