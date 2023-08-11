[English](./README.md) | 简体中文

## 简介
HoloLayer 是一个专为 Emacs 设计的多媒体图层插件， 它基于 PyQt 开发， 旨在显著增强 Emacs 的视觉体验。 插件提供了一系列的视觉效果增强功能， 包括现代化的窗口边框、 窗口跳转提示、 光标动画、 窗口截图、 透明浮动终端， 以及实时词典等等。 这些功能不仅让 Emacs 界面看起来更现代， 同时也确保了 Emacs 的运行性能不会受到影响。

## 安装
1. 安装 Emacs 28 及以上版本
2. 安装 Python 依赖: `pip3 install epc sexpdata six PyQt6 PyQt6-Qt6 PyQt6-sip` (ArchLinux 请用 pacman 来安装 PyQt6)
3. 用 `git clone` 下载此仓库， 并替换下面配置中的 load-path 路径
4. 把下面代码加入到你的配置文件 ~/.emacs 中：

```elisp
(add-to-list 'load-path "<path-to-holo-layer>")
(require 'holo-layer)
(holo-layer-enable)
```
