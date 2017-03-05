# haskell-lifegame
Haskellによるシンプルなライフゲーム

入力はASCIIテキストで表現します．`x`が生きているセルでそれ以外が死んでいるセルです．

```
___xx__
___xx__
___x___
```

## 動かし方
```shell
runhaskell lifegame.hs data/figure-eight.txt
```

## 参考文献
`data`ディレクトリ内のパターンは[LifeWiki](http://www.conwaylife.com/wiki/Main_Page)を参考にしました．
