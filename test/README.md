# 测试说明

使用 [MARS](https://courses.missouristate.edu/kenvollmar/mars/) 将测试汇编程序导出为十六进制机器码。本 CPU 采用的是 MARS 中的 Compact, Data at Address 0 地址空间， `.data` 从 `0x0000` 至 `0x2fff` ， `.text` 从 `0x3000` 至 `0x3fff` 。

```bash
java -jar Mars4_5.jar a nc mc CompactDataAtZero dump .text HexText code.txt example.asm
```

而后将 `code.txt` 放置于本项目根目录（即当前目录的上级目录）即可运行仿真。
