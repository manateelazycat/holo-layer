English | [简体中文](./README.zh-CN.md)

## Introduction
HoloLayer is a multimedia layer plugin specifically designed for Emacs. It is developed based on PyQt, aiming to significantly enhance the visual experience of Emacs. The plugin provides a series of visual enhancement features, including modern window borders, window jump prompts, cursor animations, window screenshots, transparent floating terminals, and real-time dictionaries, etc. These features not only make the Emacs interface look more modern, but also ensure that the performance of Emacs will not be affected.

## Installation
1. Install Emacs 28 or above (For MacOS, please use Emacs without fullscreen)
2. Install Python dependencies: `pip3 install epc sexpdata six PyQt6 PyQt6-Qt6 PyQt6-sip` (For ArchLinux, please use pacman to install PyQt6)
3. Use `git clone` to download this repository, and replace the load-path path in the configuration below
4. Add the following code to your configuration file ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-holo-layer>")
(require 'holo-layer)
(holo-layer-enable)
```

## Options
* holo-layer-active-window-color: The border color of the active window, only displayed when there are more than two windows
* holo-layer-inactive-window-color: The border color of the inactive window, only displayed when there are more than two windows

## Feedback Issues

For other issues, please use the command `emacs -q` and only add holo-layer configuration for a comparison test. If `emacs -q` can work normally, please check your personal configuration file.

If the problem still exists under the `emacs -q` environment, please go [here](https://github.com/manateelazycat/holo-layer/issues/new) to provide feedback, and attach the content of the `*holo-layer*` window to submit an issue to us. There are many clues in there that can help us troubleshoot the problem.

If you encounter a crash, please use the following method to collect crash information:
1. First install gdb and turn on the option `(setq holo-layer-enable-debug t)`
2. Use the command `holo-layer-stop-process` to stop the holo-layer process
3. Reopen holo-layer, and send the content of `*holo-layer*` at the next crash
