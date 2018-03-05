
# simpath - implementation of simpath, a path enumeration algorithm

This directory contains an implementation of mate-zdd, a variation of Knuth's simpath algorithm using ZDD.
The purpose of implementing this algorithm is to dogfood the cl-cudd library in a more practical setting.
This directory was originally located in https://github.com/guicho271828/simpath .

D. E. Knuth, The Art of Computer Programming:
Bitwise Tricks & Techniques; Binary Decision Dia-
grams, Vol. 4, fascicle 1, Addison-Wesley, 2009.

R. Yoshinaka, T. Saitoh, J. Kawahara, K. Tsuruma,
H. Iwashita, and S. Minato, Finding all solutions and
instances of numberlink and slitherlink by ZDDs,

JP articles:

BDD/ZDDを用いたグラフ列挙索引化技法
湊 真一

ZDD によるパスの列挙
川原 純, 斎藤 寿樹, 鈴木 拡, 湊 真一, 吉仲 亮
数理解析研究所講究録
第 1744 巻 2011 年 35-41

## Usage


## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.3.14 on X86-64 Linux 4.4.0-66-generic (author's environment)

Also, it depends on the following libraries:

+ trivia by *Masataro Asai* :
    NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase
+ cl-cudd by *Christian von Essen <christian@mvonessen.de>* :
    A two-layered binding to the CUDD binary decision diagram library.

See README.md for more details.
+ alexandria by *Nikodemus Siivola <nikodemus@sb-studio.net>, and others.* :
    Alexandria is a collection of portable public domain utilities.
+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility

## Installation

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.


