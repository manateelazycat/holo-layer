from PyQt6.QtCore import QLineF, QObject, QPointF, QTimer
from PyQt6.QtGui import QColor, QLinearGradient, QPen, QPolygonF

from utils import *


class CursorAnimation(QObject):

    def __init__(self, window) -> None:
        super().__init__()

        self.window = window

        self.cursor_color = None
        self.cursor_info = []
        self.cursor_prev_info = []
        self.cursor_timer = QTimer(self)
        self.cursor_timer.timeout.connect(self.cursor_animation_tik)
        self.cursor_animation_percent = 1

        self.cursor_info = []
        self.cursor_prev_info = []

        (self.cursor_animation_duration,
         self.cursor_animation_interval,
         self.cursor_animation_type,
         self.cursor_color_gradient,
         self.enable_cursor_animation) = get_emacs_vars(["holo-layer-cursor-animation-duration",
                                                         "holo-layer-cursor-animation-interval",
                                                         "holo-layer-cursor-animation-type",
                                                         "holo-layer-cursor-animation-color-gradient",
                                                         "holo-layer-enable-cursor-animation"])

    def update_cursor_color(self):
        (cursor_color,
         cursor_alpha) = get_emacs_vars(["holo-layer-cursor-color",
                                         "holo-layer-cursor-alpha"])
        self.cursor_color = QColor(cursor_color)
        self.cursor_color.setAlpha(cursor_alpha)

    def update_info(self, cursor_info, emacs_frame_info):
        # Don't update cursor info if cursor_info unpack failed.
        if not self.enable_cursor_animation or cursor_info is None or len(cursor_info) != 4:
            return False

        [x, y, w, h] = cursor_info
        if len(emacs_frame_info) > 1:
            cursor_info = [int(x) + int(w) + emacs_frame_info[0],
                           int(y) + emacs_frame_info[1], int(w), int(h)]
        else:
            cursor_info = [int(x) + int(w), int(y), int(w), int(h)]
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
            self.window.update()
            return

        self.cursor_animation_percent = self.elapsed_time / self.cursor_animation_duration
        self.window.update()

        self.cursor_timer.singleShot(self.cursor_animation_interval, self.cursor_animation_tik)

    def cursor_animation_draw_jelly_cursor(self, cs, ce, w, h, p):
        diff = cs - ce
        diff_x = diff.x()
        diff_y = diff.y()
        w_point = QPointF(w, 0)
        h_point = QPointF(0, h)
        wh_point = QPointF(w, h)

        points = []
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

    def cursor_animation_draw_arrow_cursor(self, cs, ce, w, h, p):
        arrow_size = 10.0

        ce = ce + QPointF(w/2, h/2)
        cs = cs + QPointF(w/2, h/2)

        p = 1 # disable animation
        pe = cs * (1 - p) + ce * p
        direction = pe - cs
        direction /= direction.manhattanLength()
        p1 = pe - arrow_size * direction
        direction = QPointF(-direction.y(), direction.x())
        p2 = p1 + arrow_size * 0.5 * direction
        p3 = p1 - arrow_size * 0.5 * direction

        return QPolygonF([pe, p2, p3]), QLineF(cs, (p2 + p3)/2)

    def cursor_animation_draw(self):
        p = self.cursor_animation_percent
        cs = self.cursor_start
        ce = self.cursor_end
        [w, h] = self.cursor_wh
        if self.cursor_animation_type == "arrow":
            return self.cursor_animation_draw_arrow_cursor(cs, ce, w, h, p)
        else:
            return self.cursor_animation_draw_jelly_cursor(cs, ce, w, h, p)

    def draw(self, painter):
        if self.cursor_animation_percent < 1 and self.enable_cursor_animation:
            if self.cursor_color is None:
                self.update_cursor_color()
            # cursor animation
            if self.cursor_animation_type == "arrow":
                painter.setPen(QPen(self.cursor_color.lighter(110), 3))
                arrow, line = self.cursor_animation_draw()
                painter.drawLines(line)
                painter.setPen(self.cursor_color)
                painter.setBrush(self.cursor_color)
                painter.drawPolygon(arrow)
            else:
                painter.setPen(self.cursor_color)

                if self.cursor_color_gradient:
                    gradient = QLinearGradient(self.cursor_start, self.cursor_end)
                    gradient.setColorAt(0.0, self.cursor_color.lighter(200))
                    gradient.setColorAt(1.0, self.cursor_color)
                    painter.setBrush(gradient)
                else:
                    painter.setBrush(self.cursor_color)

                polygon = self.cursor_animation_draw()
                painter.drawPolygon(polygon)
