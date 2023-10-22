from PyQt6.QtCore import QObject, Qt, QRectF
from PyQt6.QtGui import QColor, QPainterPath

from utils import *


class WindowBorder(QObject):

    def __init__(self) -> None:
        super().__init__()

        (active_window_border_color,
         inactive_window_border_color,
         self.enable_window_border) = get_emacs_vars(["holo-layer-active-window-color",
                                                      "holo-layer-inactive-window-color",
                                                      "holo-layer-enable-window-border"])

        self.active_window_border_color = QColor(active_window_border_color)
        self.inactive_window_border_color = QColor(inactive_window_border_color)

    def draw(self, painter, window_info, emacs_frame_info, menu_info):
        if self.enable_window_border and emacs_frame_info:
            [emacs_x, emacs_y, emacs_width, emacs_height] = emacs_frame_info

            emacs_area = QPainterPath()
            emacs_area.addRect(QRectF(emacs_x, emacs_y, emacs_width, emacs_height))

            if menu_info:
                total_mask = None

                for info in menu_info:
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

            if len(window_info) == 1:
                # Draw 1 pixel mode-line when only
                [x, y, w, h, _] = window_info[0]
                [emacs_x, emacs_y, emacs_width, emacs_height] = emacs_frame_info

                painter.setPen(self.active_window_border_color)
                painter.drawRect(x + emacs_x, y + emacs_y + h - 1, w, 1)
            elif len(window_info) > 1:
                # Draw inactive window border.
                for info in window_info:
                    [x, y, w, h, is_active_window] = info

                    if is_active_window == "nil":
                        painter.setPen(self.inactive_window_border_color)
                        self.draw_window_border(painter, emacs_frame_info, info)

                # Draw active window border.
                for info in window_info:
                    [x, y, w, h, is_active_window] = info

                    if is_active_window == "t":
                        painter.setPen(self.active_window_border_color)
                        [emacs_x, emacs_y, emacs_width, emacs_height] = emacs_frame_info

                        painter.drawRect(x + emacs_x, y + emacs_y + h - 1, w, 1)

    def draw_window_border(self, painter, emacs_frame_info, info):
        [x, y, w, h, is_active_window] = info
        [emacs_x, emacs_y, emacs_width, emacs_height] = emacs_frame_info

        if x + w >= emacs_x + emacs_width:
            # Width need -1 if window is at rightest of Emacs.
            painter.drawRect(x + emacs_x, y + emacs_y, w - 1, h)
        else:
            painter.drawRect(x + emacs_x, y + emacs_y, w, h)
