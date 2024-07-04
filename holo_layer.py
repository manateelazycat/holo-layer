#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import platform
import signal
import sys
import threading

from PyQt6.QtWidgets import QApplication, QWidget, QVBoxLayout
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor, QPainter

from PyQt6.QtCore import QRectF

from epc.server import ThreadingEPCServer
from plugin.cursor_animation import CursorAnimation
from plugin.place_info import PlaceInfo
from plugin.sort_tab import SortTab
from plugin.window_border import WindowBorder
from plugin.window_number import WindowNumber
from plugin.window_screenshot import WindowScreenshot
from plugin.indent_line import IndentLine
from plugin.type_animation import TypeAnimation
from pynput.keyboard import Listener as kbListener
from PyQt6.QtGui import QGuiApplication, QPainterPath
from utils import *

class HoloLayer:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Init vars.
        self.window_info_args = None
        self.window_info = []
        self.cursor_info_args = None
        self.menu_info_args = None
        self.is_insert_command = False
        self.cursor_info = []
        self.menu_info = []
        self.sort_tab_info = {}
        self.emacs_frame_info = None
        self.holo_window = HoloWindow()
        self.holo_window_is_show = True
        self.emacs_xid = None
        self.emacs_name = None

        # Build EPC server.
        self.server = ThreadingEPCServer(('127.0.0.1', 0), log_traceback=True)
        self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.expanduser("~/holo-layer.log"), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)
        # self.server.logger = logger

        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # Start key event listener thread.
        self.key_event_listener = threading.Thread(target=self.listen_key_event)
        self.key_event_listener.start()

        # Pass epc port and webengine codec information to Emacs when first start holo-layer.
        eval_in_emacs('holo-layer--first-start', self.server.server_address[1])

    def update_window_info(self, emacs_frame_info, window_info_args, cursor_info_args, menu_info_args, is_insert_command):
        self.is_insert_command = is_insert_command

        cursor_info_args = cursor_info_args if len(cursor_info_args) else ""
        window_info_args = window_info_args if len(window_info_args) else ""
        menu_info_args = menu_info_args if len(menu_info_args) else ""

        if window_info_args != self.window_info_args or menu_info_args != self.menu_info_args:
            self.window_info_args = window_info_args
            self.cursor_info_args = cursor_info_args
            self.emacs_frame_info = emacs_frame_info
            self.menu_info_args = menu_info_args

            if self.window_info_args == "":
                self.window_info = []
                self.cursor_info = []
            else:
                self.window_info = list(map(lambda info: info.split(":"), self.window_info_args.split(",")))
                self.cursor_info = self.cursor_info_args.split(':')

            self.menu_info = list(map(lambda info: info.split(":"), self.menu_info_args.split(",")))

            self.update()
        elif cursor_info_args != self.cursor_info_args:
            self.cursor_info_args = cursor_info_args
            self.cursor_info = self.cursor_info_args.split(':')
            self.update()

    @PostGui()
    def show_holo_window(self):
        self.holo_window_is_show = True
        self.holo_window.show_up()

    @PostGui()
    def hide_holo_window(self):
        self.holo_window_is_show = False
        self.holo_window.hide()

    @PostGui()
    def update(self):
        self.holo_window.update_info(self.emacs_frame_info, self.window_info, self.cursor_info, self.menu_info, self.sort_tab_info, self.is_insert_command)

    def update_place_info(self, word):
        self.holo_window.update_place_info(word)

    def update_indent_info(self, emacs_indent_infos):
        self.holo_window.update_indent_info(emacs_indent_infos)

    @PostGui()
    def show_window_number(self):
        self.holo_window.show_window_number()

    @PostGui()
    def hide_window_number(self):
        self.holo_window.hide_window_number()

    def take_window_screenshot(self, screenshot_window_info):
        self.screenshot_window_info = screenshot_window_info
        self.take_screenshot()

    @PostGui()
    def take_screenshot(self):
        self.holo_window.window_screenshot.take_screenshot(self.screenshot_window_info, self.emacs_frame_info)

    @PostGui()
    def render_sort_tab(self, tab_names, tab_modes, current_tab_index, current_tab_name,
                        tab_height, tab_name_max_length, emacs_frame_info,
                        emacs_theme_mode, emacs_theme_foreground_color, emacs_theme_background_color):
        self.emacs_frame_info = emacs_frame_info
        self.sort_tab_info = {
            "tab_names": tab_names,
            "tab_modes": tab_modes,
            "current_tab_index": current_tab_index,
            "current_tab_name": current_tab_name,
            "tab_height": tab_height,
            "tab_name_max_length": tab_name_max_length,
            "emacs_theme_mode": emacs_theme_mode,
            "emacs_theme_foreground_color": emacs_theme_foreground_color,
            "emacs_theme_background_color": emacs_theme_background_color
        }
        self.update()

    def listen_key_event(self):
        while True:
            with kbListener(
                    on_press=self.key_press,
                    on_release=self.key_release) as listener:
                listener.join()

    def key_press(self, key):
        pass

    def key_release(self, key):
        if self.get_active_window_id() == self.get_emacs_id():
            if not self.holo_window_is_show:
                self.show_holo_window()
        else:
            if self.holo_window_is_show:
                self.hide_holo_window()

    def get_emacs_id(self):
        if platform.system() == "Windows":
            import pygetwindow as gw
            if self.emacs_name is None:
                self.emacs_name = get_emacs_func_result("get-emacs-name")

            windows = gw.getWindowsWithTitle(self.emacs_name)
            return windows[0]._hWnd if len(windows) > 0 else None
        else:
            if self.emacs_xid is None:
                self.emacs_xid = get_emacs_func_result("get-emacs-id")

            return self.emacs_xid

    def get_active_window_id(self):
        if platform.system() == "Darwin":
            from AppKit import NSWorkspace
            return NSWorkspace.sharedWorkspace().activeApplication()['NSApplicationProcessIdentifier']
        elif platform.system() == "Windows":
            import pygetwindow as gw
            return gw.getActiveWindow()._hWnd
        else:
            from Xlib import X
            from Xlib.display import Display

            if not hasattr(self, "NET_ACTIVE_WINDOW"):
                self.disp = Display()
                self.root = self.disp.screen().root
                self.NET_ACTIVE_WINDOW = self.disp.intern_atom('_NET_ACTIVE_WINDOW')

            response = self.root.get_full_property(self.NET_ACTIVE_WINDOW, X.AnyPropertyType)
            win_id = response.value[0]

            return win_id

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

class HoloWindow(QWidget):
    def __init__(self) -> None:
        super().__init__()

        self.active_window_border_color = None
        self.inactive_window_border_color = None

        self.emacs_frame_info = None
        self.emacs_indent_infos = None
        self.menu_info = None
        self.window_info = []
        self.sort_tab_info = {}
        self.place_word = ""

        self.window_border = WindowBorder()
        self.window_number = WindowNumber()
        self.window_screenshot = WindowScreenshot()
        self.cursor_animation = CursorAnimation(self)
        self.place_info = PlaceInfo()
        self.sort_tab = SortTab()
        self.indent_line = IndentLine()

        self.show_window_number_flag = False

        self.setStyleSheet("border: none;")
        self.setContentsMargins(0, 0, 0, 0)
        self.setStyleSheet("background-color:transparent;")
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)

        self.screen_index = 0
        self.screen = QGuiApplication.primaryScreen()
        self.screen_geometry = self.screen.availableGeometry()
        self.setGeometry(self.screen_geometry)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        self.firework_view = TypeAnimation(self)
        layout.addWidget(self.firework_view)

        self.setLayout(layout)

        self.show_up()

    def show_up(self):
        if not self.isVisible():
            window_flags = Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.WindowTransparentForInput | Qt.WindowType.WindowDoesNotAcceptFocus
            if platform.system() == "Darwin":
                window_flags |= Qt.WindowType.NoDropShadowWindowHint
                self.setWindowFlags(window_flags)
                self.window_bias_x, self.window_bias_y = self.screen_geometry.x(), self.screen_geometry.y()
                self.show()
            else:
                # Why use `SplashScreen` flag for holo-layer fullsreen window?
                #
                # Because below flag can't work. ;)
                # 1. Qt.WindowType.Tool, holo-layer window coordinate will change when we do `workspace-switch` operation.
                # 2. Qt.WindowType.Popup, popup window is modal window, and it will grab keyboard to cause system can't response keyboard event
                # 3. Qt.WindowType.Tooltip or Qt.Window.X11BypassWindowManagerHint, window height can't fullscreen, window border at bottom can't render
                # 4. Qt.WindowType.SubWindow, window can't skip taskbar or alt-tab window list.
                window_flags |= Qt.WindowType.SplashScreen
                self.setWindowFlags(window_flags)
                self.window_bias_x, self.window_bias_y = 0, 0
                self.showFullScreen()

    def paintEvent(self, event):
        painter = QPainter(self)
        background_color = QColor(0, 0, 0, 0)
        painter.setBrush(background_color)
        painter.setPen(background_color)

        if self.emacs_frame_info:
            [x, y, w, h] = self.emacs_frame_info
            painter.eraseRect(x, y, w, h)
        else:
            painter.eraseRect(self.rect())

        self.sort_tab.draw(painter, self.emacs_frame_info, self.sort_tab_info)

        self.cursor_animation.draw(painter)
        painter.setBrush(background_color)
        painter.setPen(background_color)

        self.update_menu_clip_area(painter)

        self.indent_line.draw(painter, self.emacs_indent_infos, self.emacs_frame_info)

        self.window_border.draw(painter, self.window_info, self.emacs_frame_info)

        self.place_info.draw(painter, self.window_info, self.emacs_frame_info, self.place_word)

        if self.show_window_number_flag:
            self.window_number.draw(painter, self.window_info, self.emacs_frame_info)

    def update_menu_clip_area(self, painter):
        if self.emacs_frame_info:
            [emacs_x, emacs_y, emacs_width, emacs_height] = self.emacs_frame_info

            emacs_area = QPainterPath()
            emacs_area.addRect(QRectF(emacs_x, emacs_y, emacs_width, emacs_height))

            if self.menu_info:
                total_mask = None

                for info in self.menu_info:
                    try:
                        (x, y, w, h) = info
                        mask = QPainterPath()
                        mask.addRect(int(x), int(y), int(w), int(h))

                        if total_mask is None:
                            total_mask = mask
                        else:
                            total_mask += mask
                    except:
                        pass

                if total_mask is not None:
                    painter.setClipPath(emacs_area - total_mask, Qt.ClipOperation.IntersectClip)

    def update_place_info(self, word):
        word = word.lower()

        if self.place_word != word:
            self.place_word = word
            self.update()

    def update_indent_info(self, emacs_indent_infos):
        self.emacs_indent_infos = emacs_indent_infos
        self.update()

    def update_screen_geometry_info(self, screen_index):
        if platform.system() != "Darwin":
            return
        if screen_index != self.screen_index:
            self.screen_index = screen_index
            self.screen = super().screen().virtualSiblings()[screen_index]
            self.screen_geometry = self.screen.availableGeometry()
            self.window_bias_x, self.window_bias_y = self.screen_geometry.x(), self.screen_geometry.y()
            self.setGeometry(self.screen_geometry)
            self.move(self.window_bias_x, self.window_bias_y)
   
    def update_info(self, emacs_frame_info, window_info, cursor_info, menu_info, sort_tab_info, is_insert_command):
        if emacs_frame_info:
            self.emacs_frame_info = emacs_frame_info[:4].copy()
            self.update_screen_geometry_info(emacs_frame_info[4])
            self.emacs_frame_info[0] -= self.window_bias_x
            self.emacs_frame_info[1] -= self.window_bias_y

        self.menu_info = menu_info
        self.sort_tab_info = sort_tab_info

        window_info = window_info.copy()
        for i in range(len(window_info)):
            [x, y, w, h, is_active_window] = window_info[i]
            window_info[i] = [int(x), int(y), int(w), int(h), is_active_window]
        self.window_info = window_info

        if is_insert_command and self.firework_view.enable_type_animation:
            if len(cursor_info) > 1:
                firework_x = int(cursor_info[0])
                firework_y = int(cursor_info[1])

                if len(self.emacs_frame_info) > 1:
                    firework_x += self.emacs_frame_info[0]
                    firework_y += self.emacs_frame_info[1]

                self.firework_view.trigger_firework(firework_x, firework_y)

        if not self.cursor_animation.update_info(cursor_info, self.emacs_frame_info):
            # skip update if cursor position is changed.
            self.update()

    def show_window_number(self):
        if len(self.window_info) > 1:
            self.show_window_number_flag = True
            self.update()

    def hide_window_number(self):
        self.show_window_number_flag = False
        self.update()

if __name__ == "__main__":
    app = QApplication(sys.argv)
    HoloLayer(sys.argv[1:])

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec())
