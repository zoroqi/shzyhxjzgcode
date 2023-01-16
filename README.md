# shzyhxzgcode

仿照 [社会主义核心价值观编码器](https://atool.vip/corevalue/) 写的一个玩具,
    分别使用 golang 和 haskell 实现.

## 编码规则

| 二进制 | 核心价值观 |
| --- |-------|
| 0 | 富强    |
| 1 | 民主    |
| 2 | 文明    |
| 3 | 和谐    |
| 4 | 自由    |
| 5 | 平等    |
| 6 | 公正    |
| 7 | 法治    |
| 8 | 爱国    |
| 9 | 敬业    |
| 10 | 诚信富强  |
| 11 | 友善文明  |
| 12 | 友善平等  |
| 13 | 友善法治  |
| 14 | 诚信爱国  |
| 15 | 诚信友善  |


构成 16 个编码, 可以对应 4bit 的数据.

编码过程, 将读入的数据转换成 byte 数组.
每一个 byte 分割成两部分, 高四位翻译到对应"核心价值", 低四位翻译为对应"核心价值", 直接输出.

节目过程, 一次性读取 2 个汉子. 如果是"诚信"或"友善", 则再读取两个汉子.
更具字典翻译成半个 byte, 再次读取半个 byte, 将两个 halfbyte 拼接一个完整 byte, 输出就好了.

## 使用

* 编码
```
echo "abc" | shzyhxjzgcode
echo "abc" | shzyhxjzgcode -m en
shzyhxjzgcode -i input.txt -o ouput.txt
```

* 解码
```
echo "公正民主公正文明公正和谐富强诚信富强" | shzyhxjzgcode -m de
shzyhxjzgcode -m de -i input.txt -o ouput.txt
```
