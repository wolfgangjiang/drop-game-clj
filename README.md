===
Drop
===

这是一个方块消除类的游戏，它使用swing作为图形界面，全部代码都在`drop.clj`中，依赖clojure 1.3+，启动方法是：

    java cp path/to/clj/clojure-1.x.x.jar clojure.main drop.clj

这个游戏通过广度优先搜索来寻找相邻的方块，如果点击到的方块是三个同颜色的方块相邻，就消除这些同颜色的方块，并且从画面的最上方落下更多的方块，填补空间。如果画面中不存在三个或以上的同颜色的方块相邻，则游戏会自动地对方块的颜色做细微调整，使得游戏总是可以进行下去。
