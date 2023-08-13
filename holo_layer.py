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
import platform
import threading
import signal
from PyQt6.QtCore import Qt, QPointF, QTimer
from PyQt6.QtWidgets import QWidget, QApplication
from PyQt6.QtGui import QPainter, QColor, QGuiApplication, QPolygonF

from epc.server import ThreadingEPCServer
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

        self.cursor_info = []
        self.cursor_prev_info = []
        self.cursor_timer = QTimer(self)
        self.cursor_timer.timeout.connect(self.cursor_animation_tik)
        self.cursor_animation_percent = 1

        (self.cursor_animation_duration,
         self.cursor_animation_interval,
         self.enable_cursor_animation) = get_emacs_vars(["holo-layer-cursor-animation-duration",
                                                         "holo-layer-cursor-animation-interval",
                                                         "holo-layer-enable-cursor-animation"])

        self.setStyleSheet("border: none;")
        self.setContentsMargins(0, 0, 0, 0)
        self.setStyleSheet("background-color:transparent;")
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)

        if platform.system() == "Darwin":
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.WindowTransparentForInput | Qt.WindowType.WindowDoesNotAcceptFocus | Qt.WindowType.NoDropShadowWindowHint)

            # for Mac, we need to set the window to the screen size
            screen = QGuiApplication.primaryScreen()
            screen_geometry = screen.availableGeometry()
            self.window_bias_x, self.window_bias_y = screen_geometry.x(), screen_geometry.y()
            self.setGeometry(screen_geometry)
            self.show()
        else:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.WindowTransparentForInput | Qt.WindowType.WindowDoesNotAcceptFocus | Qt.WindowType.Tool)

            self.window_bias_x, self.window_bias_y = 0, 0
            self.showFullScreen()

    def paintEvent(self, event):
        if self.active_window_border_color is None:
            (active_window_border_color,
             inactive_window_border_color,
             cursor_color,
             cursor_alpha) = get_emacs_vars(["holo-layer-active-window-color",
                                             "holo-layer-inactive-window-color",
                                             "holo-layer-cursor-color",
                                             "holo-layer-cursor-alpha"])

            self.active_window_border_color = QColor(active_window_border_color)
            self.inactive_window_border_color = QColor(inactive_window_border_color)
            self.cursor_color = QColor(cursor_color)
            self.cursor_color.setAlpha(cursor_alpha)

        painter = QPainter(self)
        background_color = QColor(0, 0, 0, 0)
        painter.setBrush(background_color)
        painter.setPen(background_color)

        if self.emacs_frame_info:
            painter.drawRect(*self.emacs_frame_info)
        else:
            painter.drawRect(self.rect())

        if self.cursor_animation_percent < 1 and self.enable_cursor_animation:
            # cursor animation
            painter.setBrush(self.cursor_color)
            painter.setPen(self.cursor_color)
            polygon = self.cursor_animation_draw()
            painter.drawPolygon(polygon)
            painter.setBrush(background_color)

        if len(self.window_info) == 1:
            # Draw 1 pixel mode-line when only
            window_info = self.window_info[0]
            [x, y, w, h, _] = window_info
            [emacs_x, emacs_y, emacs_width, emacs_height] = self.emacs_frame_info

            painter.setPen(self.active_window_border_color)
            painter.drawRect(x + emacs_x, y + emacs_y + h - 1, w, 1)
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

        if x + w >= emacs_x + emacs_width:
            # Width need -1 if window is at rightest of Emacs.
            painter.drawRect(x + emacs_x, y + emacs_y, w - 1, h)
        else:
            painter.drawRect(x + emacs_x, y + emacs_y, w, h)

    def create_cursor_move_animation(self):
        [prev_x, prev_y, prev_w, prev_h] = self.cursor_prev_info
        [x, y, w, h] = self.cursor_info

        if prev_x != x or prev_y != y:
            self.cursor_start = QPointF(prev_x, prev_y)
            self.cursor_end = QPointF(x, y)
            self.cursor_wh = [w, h]

            self.elapsed_time = 0
            #self.cursor_animation_tik()
            self.cursor_timer.singleShot(self.cursor_animation_interval, self.cursor_animation_tik)
            self.cursor_prev_info = self.cursor_info

    def cursor_animation_tik(self):
        self.elapsed_time += self.cursor_animation_interval

        if self.elapsed_time >= self.cursor_animation_duration:
            self.cursor_animation_percent = 1
            self.cursor_timer.stop()
            self.update()
            return

        self.cursor_animation_percent = self.elapsed_time / self.cursor_animation_duration
        self.update()

        self.cursor_timer.singleShot(self.cursor_animation_interval, self.cursor_animation_tik)

    def cursor_animation_draw(self):
        p = self.cursor_animation_percent
        cs = self.cursor_start
        ce = self.cursor_end
        [w, h] = self.cursor_wh
        diff = cs - ce
        diff_x = diff.x()
        diff_y = diff.y()
        w_point = QPointF(w, 0)
        h_point = QPointF(0, h)
        wh_point = QPointF(w, h)

        if diff_x * diff_y > 0:
            points = [cs, cs + wh_point, ce + wh_point, ce]
        elif diff_x * diff_y < 0:
            points = [cs + h_point, cs + w_point, ce + w_point, ce + h_point]
        elif diff_x == 0:
            if diff_y >= 0:
                points = [cs + h_point, cs + wh_point, ce + w_point, ce]
            else:
                points = [cs, cs + w_point, ce + wh_point, ce + h_point]
        elif diff_y == 0:
            if diff_x >= 0:
                points = [cs + w_point, cs + wh_point, ce + h_point, ce]
            else:
                points = [cs, cs + h_point, ce + wh_point, ce + w_point]

        if p < 0.5:
            points[2] = points[2] * p * 2 + points[1] * (1 - p * 2)
            points[3] = points[3] * p * 2 + points[0] * (1 - p * 2)
        else:
            points[0] = points[3] * (p - 0.5) * 2 + points[0] * (1 - (p - 0.5) * 2)
            points[1] = points[2] * (p - 0.5) * 2 + points[1] * (1 - (p - 0.5) * 2)

        return QPolygonF(points)

    def update_cursor_info(self, cursor_info):
        # Don't update cursor info if cursor_info unpack failed.
        try:
            [x, y, w, h] = cursor_info
        except:
            return

        [x, y, w, h] = cursor_info
        if len(self.emacs_frame_info) > 1:
            cursor_info = [int(x) - self.window_bias_x + int(w) + self.emacs_frame_info[0],
                            int(y) - self.window_bias_y + self.emacs_frame_info[1],
                            int(w), int(h)]
        else:
            cursor_info = [int(x) - self.window_bias_x + int(w), int(y) - self.window_bias_y,
                            int(w), int(h)]
        [x, y, w, h] = cursor_info

        self.cursor_info = cursor_info
        if len(self.cursor_prev_info) > 1 and \
           (self.cursor_prev_info[0] != self.cursor_info[0] or
            self.cursor_prev_info[1] != self.cursor_info[1]):

            self.create_cursor_move_animation()
            return True
        elif len(self.cursor_prev_info) == 0:
            self.cursor_prev_info = self.cursor_info
            return False
        else:
            self.cursor_prev_info = self.cursor_info
            self.cursor_animation_percent = 1
            self.cursor_timer.stop()
            return False


    def update_info(self, emacs_frame_info, window_info, cursor_info):
        emacs_frame_info[0] -= self.window_bias_x
        emacs_frame_info[1] -= self.window_bias_y
        for i in range(len(window_info)):
            [x, y, w, h, is_active_window] = window_info[i]
            window_info[i] = [int(x) - self.window_bias_x, int(y) - self.window_bias_y,
                              int(w), int(h), is_active_window]

        self.emacs_frame_info = emacs_frame_info
        self.window_info = window_info

        if not self.update_cursor_info(cursor_info):
            # skip update if cursor position is changed.
            self.update()

if __name__ == "__main__":
    app = QApplication(sys.argv)
    HoloLayer(sys.argv[1:])

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec())
