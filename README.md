# MinRuby

Ascii.jpで連載されている[Rubyで学ぶRuby](http://ascii.jp/elem/000/001/230/1230449/)について

- 各回の演習問題(実装寄りのも)を解く
- [Ruby](https://raw.githubusercontent.com/mame/minruby/master/lib/minruby.rb) 以外のパーサーを書く
- Ruby 以外の言語でも MinRuby を作成
	- 少しずつ 

## パーサーについて

連載でのパーサー部分(文字列から構文木への変換器)は事前に与えられている。

なので、初級者が Ruby 以外の言語で書こうとしても、パーサーが無いため書けない。

そのために、いろんな言語用の MinRuby パーサーを書いてみる。
