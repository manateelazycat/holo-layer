[English](./README.md) | 简体中文

## 简介
HoloLayer 是一个专为 Emacs 设计的多媒体图层插件， 它基于 PyQt 开发， 旨在显著增强 Emacs 的视觉体验。 插件提供了一系列的视觉效果增强功能， 包括现代化的窗口边框、 窗口跳转提示、 光标动画、 窗口截图、 透明浮动终端， 以及实时词典等等。 这些功能不仅让 Emacs 界面看起来更现代， 同时也确保了 Emacs 的运行性能不会受到影响。

## 安装
1. 安装 Emacs 28 及以上版本 
2. 安装 Python 依赖: 
 - Linux: `pip3 install epc sexpdata six pynput inflect PyQt6 PyQt6-Qt6 PyQt6-sip` (ArchLinux 请用 pacman 来安装 PyQt6)
 - Windows: `pip3 install epc sexpdata six pynput inflect pygetwindow PyQt6 PyQt6-Qt6 PyQt6-sip`
 - macOS: `pip3 install epc sexpdata six pynput inflect pyobjc PyQt6 PyQt6-Qt6 PyQt6-sip`
3. 用 `git clone` 下载此仓库， 并替换下面配置中的 load-path 路径
4. 把下面代码加入到你的配置文件 ~/.emacs 中：

```elisp
(add-to-list 'load-path "<path-to-holo-layer>")
(require 'holo-layer)
(holo-layer-enable)
```

备注： 
* MacOS 请使用窗口模式
* 如果你用 Conda 或者其他虚拟环境， 请先设置 `holo-layer-python-command` 为对应的 Python 路径。

## 演示效果

### 果冻光标动画
<p align="center">
  <img style='height: auto; width: 80%; object-fit: contain' src="./demo/jelly-cursor.gif">
</p>

### 彩虹缩进辅助线
<p align="center">
  <img style='height: auto; width: 80%; object-fit: contain' src="./demo/indent-line.png">
</p>

### 现代化的标签栏
<p align="center">
  <img style='height: auto; width: 80%; object-fit: contain' src="./demo/sort-tab-ui.png">
</p>

需要先安装 [sort-tab](https://github.com/manateelazycat/sort-tab), 再打开选项 `holo-layer-sort-tab-ui`

所有图标都放在子目录 icon_cache 下面， 欢迎贡献你认为好看的图标， 图标可以从 https://devicon.dev 或 https://icons8.com/icons/set/programming-language 这样的网站去查找。

修改 svg 图标颜色， 只用在 path 下增加 `style="fill: hex-color"` 属性即可。

## 选项
* holo-layer-enable-cursor-animation: 是否开启果冻光标的动画， 默认是关闭的
* holo-layer-enable-type-animation: 是否开启光标敲击动画， 默认是关闭的
* holo-layer-enable-indent-rainbow: 是否开启彩虹缩进功能， 默认是关闭的
* holo-layer-enable-place-info: 在屏幕右上角显示光标处信息， 比如光标处单词的翻译， 默认关闭
* holo-layer-enable-window-border: 显示窗口边框, 默认关闭
* holo-layer-hide-mode-line: 打开这个选项隐藏 mode-line， 默认是关闭的
* holo-layer-active-window-color: 激活窗口的边框颜色， 只在超过两个窗口的时候显示
* holo-layer-inactive-window-color: 非激活窗口的边框颜色， 只在超过两个窗口的时候显示
* holo-layer-cursor-color: 果冻光标的颜色， 默认为 nil， 会跟随 Emacs 当前的光标背景色动态变化， 也可以设置颜色避免动态变换
* holo-layer-cursor-alpha: 果冻光标的 Alpha 透明度， 默认是 200， 完全透明是 0， 不透明是 255 
* holo-layer-cursor-animation-interval: 果冻光标动画的间隔时间， 默认是 10ms， 不用担心时间间隔， 因为动画效果是用 PyQt 多线程绘制的， 不影响 Emacs 性能
* holo-layer-cursor-block-commands: 如果在某些情况下你不希望显示光标动画， 可以将命令字符串添加到这个列表中, 比如 `self-insert-command` 加入列表后， 插入字符不会产生动画
* holo-layer-cursor-animation-color-gradient: 光标动画的渐变颜色， 默认开启
* holo-layer-cursor-animation-color-gradient-start-value: 光标动画的渐变颜色的起始颜色， 会比光标背景色浅 50 个单位
* holo-layer-type-style: 光标敲击动画的风格， 可以设置为 `"flame"` 或者 `"firework"`, 默认是 `"flame"`
* holo-layer-place-info-text-color: 光标处信息的文字颜色， 默认用的`default`的前景色
* holo-layer-place-info-background-color: 光标处信息的背景颜色， 默认用的`default`的背景色
* holo-layer-place-info-font-size: 光标处信息的字体大小， 默认是 18
* holo-layer-window-number-color: 窗口跳转数字的颜色, 用 `holo-layer-jump-to-window` 命令跳转窗口
* holo-layer-window-number-font-size: 窗口跳转数字的字体大小， 默认是 40
* holo-layer-sort-tab-ui: 是否渲染现代标签栏， 默认是 nil, 需要先安装 [sort-tab](https://github.com/manateelazycat/sort-tab)
* holo-layer-sort-tab-font-size: 标签栏的文字大小

## Hyprland
使用 Hyprland 请添加如下配置: 

```
windowrulev2 = float,title:(holo_layer.py)
windowrulev2 = nofocus,title:(holo_layer.py)
windowrulev2 = noblur,title:(holo_layer.py)
windowrulev2 = fakefullscreen,title:(holo_layer.py)
```

## 反馈问题

关于其他问题， 请用命令 `emacs -q` 并只添加 holo-layer 配置做一个对比测试， 如果 `emacs -q` 可以正常工作， 请检查你个人的配置文件。

如果`emacs -q`环境下问题依旧， 请到[这里](https://github.com/manateelazycat/holo-layer/issues/new) 反馈, 并附带 `*holo-layer*` 窗口的内容给我们提交 issue， 那里面有很多线索可以帮助我们排查问题。

如果你遇到崩溃的问题, 请用下面的方式来收集崩溃信息:
1. 先安装 gdb 并打开选项 `(setq holo-layer-enable-debug t)`
2. 使用命令 `holo-layer-stop-process` 停止 holo-layer 进程
3. 重新打开 holo-layer, 并在下次崩溃时发送 `*holo-layer*` 的内容

## 贡献者
<a href = "https://github.com/manateelazycat/holo-layer/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/holo-layer"/>
</a>

