from PyQt6.QtCore import QObject, QRectF, Qt
from PyQt6.QtGui import QColor, QFont, QFontDatabase
from utils import get_emacs_vars


class WindowNumber(QObject):

    def __init__(self) -> None:
        super().__init__()

        self.text_color = None

        (text_color, self.font_size, self.enable_background) = get_emacs_vars(["holo-layer-window-number-color",
                                                                               "holo-layer-window-number-font-size",
                                                                               "holo-layer-enable-window-number-background",
                                                                               ])

        self.text_color = QColor(text_color)
        self.font_family = QFontDatabase.systemFont(
            QFontDatabase.SystemFont.FixedFont
        ).family()
        self.font = QFont()
        self.font.setFamily(self.font_family)
        self.font.setPointSize(self.font_size)

        self.margin_left = 20
        self.back_color = QColor(125,125,125,70)

    def draw(self, painter, window_info, emacs_frame_info):
        if len(window_info) > 1:
            [emacs_x, emacs_y, emacs_w, emacs_h] = emacs_frame_info
            for index, info in enumerate(sorted(window_info, key=lambda window: (window[1], window[0]))):
                [x, y, w, h, is_active_window] = info
                
                if self.enable_background:
                    painter.setPen(self.back_color)
                    painter.setBrush(self.back_color)
                    painter.drawRect(QRectF(emacs_x + x, emacs_y + y, self.font_size + self.margin_left + 10, self.font_size + 10))
                
                painter.setFont(self.font)
                painter.setPen(self.text_color)
                painter.drawText(QRectF(emacs_x + x + self.margin_left, emacs_y + y, w, h), Qt.AlignmentFlag.AlignLeft, str(index + 1))
