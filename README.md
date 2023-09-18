# MIPS CPU

用 SpinalHDL 复刻 MIPS 五级流水线。

## 目录结构

- `src/main/scala`: 源代码目录
- `src/test/scala`: 仿真测试代码目录
- `verilog`: 生成的 Verilog 代码目录

## 流水线设计

经典 32 位 MIPS 五级流水线，跳转指令有延迟槽，在 D 级生效；乘除法器位于 E 级，用计数器模拟乘除延迟；访存在 M 级，寄存器回写在 W 级。 指令存储器 (IM) 和数据存储器 (DM) 位于 CPU 外部，CPU 自身通过顶层接口来取指令和读写存储器。

数据通路和指令分离，数据通路只包括指令计数器, 通用寄存器堆, 乘除法器等状态部件和流水线寄存器、转发阻塞单元。 其余所有的组合逻辑如 ALU，比较与跳转、访存地址生成和数据处理等均在指令类中定义，且由软件来生成。 （便于新增指令，但硬件资源消耗很大）

## 指令建模

一个指令一个类，包含：

- 指令编码
  - 识别码，`M` 类型 SpinalHDL 字面量，用于译码得到指令种类
- 转发暂停信息
  - 在哪一级用到寄存器数据
  - 在哪一级生成寄存器数据
- 在每个流水级的行为
  - 与流水寄存器中流水的数据接口交互
  - 每个指令在每个流水级相当于一个组合逻辑模块

然后用软件对每个指令在数据通路中生成相应的组合逻辑电路，并根据当前的指令种类以 MUX 选择指令结果。用 `when` 描述条件分支，利用多次 `:=` 赋值取最后一次的特性，构建 MUX 电路。

当前 CPU 支持的指令集为 MIPS32 的如下子集（不含中断异常行为）：

```
add, sub, and, or, slt, sltu
addi, addiu, andi, ori, lui
lw, lh, lb, sw, sh, sb
beq, bne
j, jal, jr
mult, multu, div, divu, mfhi, mflo, mthi, mtlo
```

## 仿真测试

- 用 Scala 编写 "testbench" 并建模 IM 与 DM
- 调用 verilator/iverilog 或 vcs 作为后端运行仿真

## 运行

要生成 Verilog 代码，直接在终端执行以下命令（如果是首次运行，sbt 将联网自动下载依赖）：

```bash
sbt "runMain CPU"
```

生成的 Verilog 代码位于 `verilog/CPU.v` 。

要运行仿真，首先需要安装 Verilator/Icarus Verilog/Synopsys VCS 三种仿真工具之一，并准备好要让 CPU 执行的程序机器码文件 `code.txt` （每行为一条指令的十六进制机器码），而后在终端执行以下命令（以 Verilator 仿真工具为例，如使用后两种仿真工具，请将 `verilator` 替换为 `iverilog` 或 `vcs`）：

```bash
sbt "Test/runMain Sim verilator"
```

仿真过程会输出写寄存器和写内存的行为序列，输出到终端以及 `output.txt` 中。
