# 2022-11-29

真的是一步一个坎啊.
本以为会在最后处理输入, 输出和命令行参数卡主,
    但是发现实现 decode 就卡主了.
在处理读取中文字符的时候, 好像要用到 State Monad.
问题是这个再看 rwh 的时候也没有完全理解,
    本以为不会这么快就用到这个知识点,
    但竟然这么快可能要用上, 这就有点累了.

今天遇到的另一个问题和读取文本有关.
不知道如何读取 pipeline, 所以改成了先读取文件的策略.
读取文件最开始用用的是 Data.ByteString 中的 fromFilePath,
    但是这个读不出来, 只是把文件名给输出了,
    之后换了一个 readFile 好了.
之后在看咋搞吧.

最后一个问题是, 我在实现的时候需要把 encode 的返回值变成 `IO String` 吗?
现在我是在 main 里面加了一个 return 来处理的, 不知道这样好不好.

至少今天实现的 encode 逻辑基本是对的, encode 后的内容可以用 golang 的代码进行 decode

# 2022-12-06

尝试通过 stdin 读取输入, 直接使用 getLine 作为尝试,
    可以实现读取, 但只能读取一行.
我可以切换到 getContents , 但是这个是一个严格求值的函数, 我想找一个惰性求值的.

发现 ByteString 包有一个 interact 可以尝试看看, 一个直接的管道函数, 这个就比较简单了.
但这个输出有问题, ByteString 是 Word8 的只能输出 ASCII 部分的内容, 超出部分会乱码.
尝试用别的包里的 interact 来替换看看, 使用 System.IO 里的就好了.

现在问题是, 我可以读写指定文件, 也可以使用 stdin/stdout, 但不能混合使用.
这个不知道咋解决的, golang 我用了一个狗血的方案来实现这个逻辑兼容, 但这里不知道咋做了.
还有就是狗血的文件错误问题了, 那个也不知道咋处理.

# 2022-12-10

写 decode 最费劲的是如何读取字符串根据要求转换成对应的 Word8.
需要一次读取 2 到 4 个字符, 在 golang 中这个逻辑很好写, 也很好实现.
但 haskell 这个逻辑不知道咋实现了.

用了一个很别扭的方式实现了解码, 但是真的很丑.
通过多个拆分合并逻辑实现了对字符动态处理
尝试用 State Mode 但是没有看懂.
我几乎跳过了所有有问题的部分, 尽量向后处理.
我甚至都想不到一个好的函数名了,
    这里起名字是真的费劲.

# 2022-12-12

先使用 real world haskell 中关于命令行参数解析的工具. 在写完成后再看看切换到 optparse-applicative 库再看看效果.
粘的文档的内容, 就是没有成功, 蛋疼.
无法实现对参数的解析, 不知道里边的处理

# 2022-12-13

我知道为啥了, 在使用参数的时候很不习惯. 短选项不能有空格, 全称要用等号做关联.
但自己想象 java 的黑魔法好像也是这种规则.
在 golang 中 flags 好像兼容了这些情况,
    测试后发现不可以.
这个问题卡了我 1h, 我对最传统命令行规范不熟啊.
我更习惯空格的方案.

```sh
// haskell 中的写法
... -mde
// golang 中的写法
... -m de / -m
```

# 2022-12-22

发现之前只是粘了代码没看懂, 现在看看什么意思

```haskell
Maybe String -> a
(\f opts -> opts { optMode = Just f }) . fromMaybe "en"

---

fromMaybe :: a -> Maybe a -> a
fromMaybe "en"
-- 声明: Maybe String -> String
---

(\f opts -> opts{optMode = Just f})
-- 声明: Maybe String -> (Options -> Options)

---
(\f opts -> opts { optMode = Just f }) . fromMaybe "en"
-- 声明: Maybe String -> (Options -> Options)
```

用 readFile 和 writeFile 实现文件读写.
感觉有点别扭.
do, let, where 三者组合的作用域我有点混乱.
where 中 do 语句返回也有点混乱,
    最后让我定义 pipe 函数的时候不得不多追加一个参数.
我是想让 pipe 直接返回一个 `(String -> String) -> IO ()`, 这样的函数
    但不知道为什么我必须把 `(String -> String)` 写在实现中, 无法理解.
之后看看为啥吧.

# 2022-12-26

反复测试后我知道了, 我知道为什么要在 pipe 依赖一个函数生命出来.

```haskell
(Just i, Just o) -> do
    s <- readFile i
    return (\f -> writeFile o (f s))
```

这个写法最后返回的结果是 `IO ((String -> String) -> IO ())` 这么一个类型.
不写 return 会报错, do 语句必须返回一个 Monad , 不写 return 返回的是一个 (String -> String) -> IO() 无法检测通过的.

但是这么定义, 所有 interact 也需要用 return 进行包裹, 这个有点麻烦.
还是写 f 吧. 看了 interact 也是这么做的.
