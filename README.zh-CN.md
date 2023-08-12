[English](./README.md) | 简体中文

## 简介
HoloLayer 是一个专为 Emacs 设计的多媒体图层插件， 它基于 PyQt 开发， 旨在显著增强 Emacs 的视觉体验。 插件提供了一系列的视觉效果增强功能， 包括现代化的窗口边框、 窗口跳转提示、 光标动画、 窗口截图、 透明浮动终端， 以及实时词典等等。 这些功能不仅让 Emacs 界面看起来更现代， 同时也确保了 Emacs 的运行性能不会受到影响。

## 安装
1. 安装 Emacs 28 及以上版本 (MacOS 请使用窗口模式)
2. 安装 Python 依赖: `pip3 install epc sexpdata six PyQt6 PyQt6-Qt6 PyQt6-sip` (ArchLinux 请用 pacman 来安装 PyQt6)
3. 用 `git clone` 下载此仓库， 并替换下面配置中的 load-path 路径
4. 把下面代码加入到你的配置文件 ~/.emacs 中：

```elisp
(add-to-list 'load-path "<path-to-holo-layer>")
(require 'holo-layer)
(holo-layer-enable)
```

## 选项
* holo-layer-active-window-color: 激活窗口的边框颜色， 只在超过两个窗口的时候显示
* holo-layer-inactive-window-color: 非激活窗口的边框颜色， 只在超过两个窗口的时候显示

## 反馈问题

关于其他问题， 请用命令 `emacs -q` 并只添加 holo-layer 配置做一个对比测试， 如果 `emacs -q` 可以正常工作， 请检查你个人的配置文件。

如果`emacs -q`环境下问题依旧， 请到[这里](https://github.com/manateelazycat/holo-layer/issues/new) 反馈, 并附带 `*holo-layer*` 窗口的内容给我们提交 issue， 那里面有很多线索可以帮助我们排查问题。

如果你遇到崩溃的问题, 请用下面的方式来收集崩溃信息:
1. 先安装 gdb 并打开选项 `(setq holo-layer-enable-debug t)`
2. 使用命令 `holo-layer-stop-process` 停止 holo-layer 进程
3. 重新打开 holo-layer, 并在下次崩溃时发送 `*holo-layer*` 的内容
