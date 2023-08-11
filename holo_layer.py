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
import sys
import threading
import signal
from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QWidget, QApplication
from PyQt6.QtGui import QPainter, QColor

from epc.server import ThreadingEPCServer
from utils import *


class HoloLayer:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Init vars.
        self.window_info_args = None
        self.window_info = []
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

    def update_window_info(self, emacs_frame_info, window_info_args):
        if window_info_args != self.window_info_args:
            self.window_info_args = window_info_args
            self.emacs_frame_info = emacs_frame_info

            if self.window_info_args == "":
                self.window_info = []
            else:
                self.window_info = list(map(lambda info: info.split(":"),
                                            self.window_info_args.split(",")))
            self.update()

    @PostGui()
    def update(self):
        self.holo_window.update_info(self.emacs_frame_info, self.window_info)

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

        self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.Tool | Qt.WindowType.WindowDoesNotAcceptFocus | Qt.WindowType.WindowTransparentForInput)

        self.setStyleSheet("border: none;")
        self.setContentsMargins(0, 0, 0, 0)

        # Make sure window transparent.
        self.setStyleSheet("background-color:transparent;")
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)

        self.showFullScreen()

    def paintEvent(self, event):
        if self.active_window_border_color is None:
            (active_window_border_color,
             inactive_window_border_color) = get_emacs_vars(["holo-layer-active-window-color",
                                                             "holo-layer-inactive-window-color"])

            self.active_window_border_color = QColor(active_window_border_color)
            self.inactive_window_border_color = QColor(inactive_window_border_color)

        painter = QPainter(self)
        background_color = QColor(0, 0, 0, 0)
        painter.setBrush(background_color)
        painter.setPen(background_color)

        if self.emacs_frame_info:
            painter.drawRect(*self.emacs_frame_info)
        else:
            painter.drawRect(self.rect())

        if len(self.window_info) == 1:
            # Draw 1 pixel mode-line when only
            window_info = self.window_info[0]
            [x, y, w, h, _] = window_info
            [emacs_x, emacs_y, emacs_width, emacs_height] = self.emacs_frame_info

            painter.setPen(self.active_window_border_color)
            painter.drawRect(int(x) + emacs_x, int(y) + emacs_y + int(h) - 1, int(w), 1)
        elif len(self.window_info) > 1:
            # Draw inactive window border.
            for info in self.window_info:
                [x, y, w, h, is_active_window] = info

                if is_active_window == "nil":
                    painter.setPen(self.inactive_window_border_color)
                    self.draw_window_border(painter, info)

            # Draw active window border.
            for info in self.window_info:
                [x, y, w, h, is_active_window] = info

                if is_active_window == "t":
                    painter.setPen(self.active_window_border_color)
                    self.draw_window_border(painter, info)

    def draw_window_border(self, painter, info):
        [x, y, w, h, is_active_window] = info
        [emacs_x, emacs_y, emacs_width, emacs_height] = self.emacs_frame_info

        if int(x) + int(w) >= emacs_x + emacs_width:
            # Width need -1 if window is at rightest of Emacs.
            painter.drawRect(int(x) + emacs_x, int(y) + emacs_y, int(w) - 1, int(h))
        else:
            painter.drawRect(int(x) + emacs_x, int(y) + emacs_y, int(w), int(h))

    def update_info(self, emacs_frame_info, window_info):
        self.emacs_frame_info = emacs_frame_info
        self.window_info = window_info
        self.update()

if __name__ == "__main__":
    app = QApplication(sys.argv)
    HoloLayer(sys.argv[1:])

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec())
