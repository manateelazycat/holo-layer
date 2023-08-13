from PyQt6.QtCore import QObject, QRectF, Qt
from PyQt6.QtGui import QColor, QFont, QFontDatabase
from utils import get_emacs_vars


class WindowNumber(QObject):

    def __init__(self) -> None:
        super().__init__()

        self.text_color = None

        (text_color, self.font_size) = get_emacs_vars(["holo-layer-window-number-color",
                                                       "holo-layer-window-number-font-size"])

        self.text_color = QColor(text_color)
        self.font_family = QFontDatabase.systemFont(
            QFontDatabase.SystemFont.FixedFont
        ).family()
        self.font = QFont()
        self.font.setFamily(self.font_family)
        self.font.setPointSize(self.font_size)

        self.margin_left = 20

    def draw(self, painter, window_info):
        if len(window_info) > 1:
            for index, info in enumerate(sorted(window_info, key=lambda window: (window[1], window[0]))):
                [x, y, w, h, is_active_window] = info

                painter.setFont(self.font)

                painter.setPen(self.text_color)
                painter.drawText(QRectF(x + self.margin_left, y, w, h), Qt.AlignmentFlag.AlignLeft, str(index + 1))
