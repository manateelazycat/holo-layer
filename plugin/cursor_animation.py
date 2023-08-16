from PyQt6.QtCore import QLineF, QObject, QPointF, QTimer, QEasingCurve
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
            cursor_info = [int(x) + emacs_frame_info[0],
                           int(y) + emacs_frame_info[1], int(w), int(h)]
        else:
            cursor_info = [int(x), int(y), int(w), int(h)]

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
            self.cursor_start_wh = [prev_w, prev_h]
            self.cursor_end_wh = [w, h]

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

    def cursor_animation_draw_jelly_cursor(self, cs, ce, ws, hs, we, he, p):
        diff = cs - ce
        diff_x = diff.x()
        diff_y = diff.y()
        ws_point = QPointF(ws, 0)
        hs_point = QPointF(0, hs)
        we_point = QPointF(we, 0)
        he_point = QPointF(0, he)
        whs_point = QPointF(ws, hs)
        whe_point = QPointF(we, he)

        points = []
        if diff_x * diff_y > 0:
            points = [cs, cs + whs_point, ce + whe_point, ce]
        elif diff_x * diff_y < 0:
            points = [cs + hs_point, cs + ws_point, ce + we_point, ce + he_point]
        elif diff_x == 0:
            if diff_y >= 0:
                points = [cs + hs_point, cs + whs_point, ce + we_point, ce]
            else:
                points = [cs, cs + ws_point, ce + whe_point, ce + he_point]
        elif diff_y == 0:
            if diff_x >= 0:
                points = [cs + ws_point, cs + whs_point, ce + he_point, ce]
            else:
                points = [cs, cs + hs_point, ce + whe_point, ce + we_point]

        if p < 0.5:
            points[2] = points[2] * p * 2 + points[1] * (1 - p * 2)
            points[3] = points[3] * p * 2 + points[0] * (1 - p * 2)
        else:
            points[0] = points[3] * (p - 0.5) * 2 + points[0] * (1 - (p - 0.5) * 2)
            points[1] = points[2] * (p - 0.5) * 2 + points[1] * (1 - (p - 0.5) * 2)

        return QPolygonF(points)

    def cursor_animation_draw_jelly_easing_cursor(self, cs, ce, ws, hs, we, he, p):
        diff = ce - cs
        diff_x = diff.x()
        diff_y = diff.y()
        start = [cs, cs + QPointF(ws, 0), cs + QPointF(ws, hs), cs + QPointF(0, hs)]
        end = [ce, ce + QPointF(we, 0), ce + QPointF(we, he), ce + QPointF(0, he)]

        easing = QEasingCurve(QEasingCurve.Type.InOutCubic)
        def easing_clamp(x):
            return easing.valueForProgress(max(0, (min(x, 1))))
        p_slow = easing_clamp(5 / 3 * (p - 0.4))
        p_norm = easing_clamp(5 / 3 * (p - 0.2))
        p_fast = easing_clamp(5 / 3 * p)

        if diff_x == 0:
            if diff_y >= 0:
                ps = [p_slow, p_slow, p_fast, p_fast]
            else:
                ps = [p_fast, p_fast, p_slow, p_slow]
        elif diff_y == 0:
            if diff_x >= 0:
                ps = [p_slow, p_fast, p_fast, p_slow]
            else:
                ps = [p_fast, p_slow, p_slow, p_fast]
        elif diff_x >=0:
            if diff_y >=0:
                ps = [p_slow, p_norm, p_fast, p_norm]
            else:
                ps = [p_norm, p_fast, p_norm, p_slow]
        else:
            if diff_y >= 0:
                ps = [p_norm, p_slow, p_norm, p_fast]
            else:
                ps = [p_fast, p_norm, p_slow, p_norm]

        points = [
            p * e + (1 - p) * s
            for p, s, e in zip(ps, start, end)
        ]

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
        [ws, hs] = self.cursor_start_wh
        [we, he] = self.cursor_end_wh
        if self.cursor_animation_type == "arrow":
            return self.cursor_animation_draw_arrow_cursor(cs, ce, ws, hs, p)
        elif self.cursor_animation_type == "jelly easing":
            return self.cursor_animation_draw_jelly_easing_cursor(cs, ce, ws, hs, we, he, p)
        else:
            return self.cursor_animation_draw_jelly_cursor(cs, ce, ws, hs, we, he, p)

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
