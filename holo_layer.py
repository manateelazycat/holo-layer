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

from epc.server import ThreadingEPCServer
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor, QGuiApplication, QPainter
from PyQt6.QtWidgets import QApplication, QWidget

from plugin.cursor_animation import CursorAnimation
from plugin.place_info import PlaceInfo
from plugin.window_border import WindowBorder
from utils import *


class HoloLayer:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Init vars.
        self.window_info_args = None
        self.window_info = []
        self.cursor_info_args = None
        self.cursor_info = []
        self.emacs_frame_info = None
        self.holo_window = HoloWindow()

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

        # Pass epc port and webengine codec information to Emacs when first start holo-layer.
        eval_in_emacs('holo-layer--first-start', self.server.server_address[1])

    def update_window_info(self, emacs_frame_info, window_info_args, cursor_info_args):
        cursor_info_args = cursor_info_args if len(cursor_info_args) else ""
        window_info_args = window_info_args if len(window_info_args) else ""

        if window_info_args != self.window_info_args:
            self.window_info_args = window_info_args
            self.cursor_info_args = cursor_info_args
            self.emacs_frame_info = emacs_frame_info

            if self.window_info_args == "":
                self.window_info = []
                self.cursor_info = []
            else:
                self.window_info = list(map(lambda info: info.split(":"),
                                            self.window_info_args.split(",")))
                self.cursor_info = self.cursor_info_args.split(':')
            self.update()
        elif cursor_info_args != self.cursor_info_args:
            self.cursor_info_args = cursor_info_args
            self.cursor_info = self.cursor_info_args.split(':')
            self.update()

    @PostGui()
    def update(self):
        self.holo_window.update_info(self.emacs_frame_info, self.window_info, self.cursor_info)

    def update_place_info(self, word):
        self.holo_window.update_place_info(word)

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

class HoloWindow(QWidget):
    def __init__(self) -> None:
        super().__init__()

        self.active_window_border_color = None
        self.inactive_window_border_color = None

        self.emacs_frame_info = None
        self.window_info = []
        self.place_word = ""

        self.window_border = WindowBorder()
        self.cursor_animation = CursorAnimation(self)
        self.place_info = PlaceInfo()

        self.setStyleSheet("border: none;")
        self.setContentsMargins(0, 0, 0, 0)
        self.setStyleSheet("background-color:transparent;")
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)

        screen = QGuiApplication.primaryScreen()
        screen_geometry = screen.availableGeometry()
        self.setGeometry(screen_geometry)

        if platform.system() == "Darwin":
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.WindowTransparentForInput | Qt.WindowType.WindowDoesNotAcceptFocus | Qt.WindowType.NoDropShadowWindowHint)

            # for Mac, we need to set the window to the screen size
            self.window_bias_x, self.window_bias_y = screen_geometry.x(), screen_geometry.y()
            self.show()
        else:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.WindowTransparentForInput | Qt.WindowType.WindowDoesNotAcceptFocus | Qt.WindowType.Tool)

            self.window_bias_x, self.window_bias_y = 0, 0
            self.showFullScreen()

    def paintEvent(self, event):
        painter = QPainter(self)
        background_color = QColor(0, 0, 0, 0)
        painter.setBrush(background_color)
        painter.setPen(background_color)

        if self.emacs_frame_info:
            painter.drawRect(*self.emacs_frame_info)
        else:
            painter.drawRect(self.rect())


        self.cursor_animation.draw(painter)
        painter.setBrush(background_color)
        painter.setPen(background_color)

        self.window_border.draw(painter, self.window_info, self.emacs_frame_info)

        self.place_info.draw(painter, self.window_info, self.emacs_frame_info, self.place_word)

    def update_place_info(self, word):
        word = word.lower()

        if self.place_word != word:
            self.place_word = word
            self.update()

    def update_info(self, emacs_frame_info, window_info, cursor_info):
        self.emacs_frame_info = emacs_frame_info.copy()
        self.emacs_frame_info[0] -= self.window_bias_x
        self.emacs_frame_info[1] -= self.window_bias_y

        window_info = window_info.copy()
        for i in range(len(window_info)):
            [x, y, w, h, is_active_window] = window_info[i]
            window_info[i] = [int(x), int(y), int(w), int(h), is_active_window]
        self.window_info = window_info

        if not self.cursor_animation.update_info(cursor_info, self.emacs_frame_info):
            # skip update if cursor position is changed.
            self.update()

if __name__ == "__main__":
    app = QApplication(sys.argv)
    HoloLayer(sys.argv[1:])

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec())
