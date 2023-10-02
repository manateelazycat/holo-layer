from PyQt6.QtCore import QObject, Qt, QRectF
from PyQt6.QtGui import QColor, QFontMetrics, QFontDatabase, QFont

from utils import *

class SortTab(QObject):

    def __init__(self) -> None:
        super().__init__()

        [self.font_size] = get_emacs_vars([
            "holo-layer-sort-tab-font-size"
        ])

        self.font_family = QFontDatabase.systemFont(
            QFontDatabase.SystemFont.FixedFont
        ).family()
        self.font = QFont()
        self.font.setFamily(self.font_family)
        self.font.setPointSize(self.font_size)

    def draw(self, painter, emacs_frame_info, sort_tab_info):
        painter.save()

        print("***** ", emacs_frame_info, sort_tab_info)

        if "emacs_theme_mode" in sort_tab_info and emacs_frame_info:
            theme_mode = sort_tab_info["emacs_theme_mode"]
            theme_foreground_color = sort_tab_info["emacs_theme_foreground_color"]
            theme_background_color = sort_tab_info["emacs_theme_background_color"]

            if theme_mode == "dark":
                if theme_background_color == "#000000":
                    tab_background_color = "#333333"
                else:
                    tab_background_color = QColor(theme_background_color).darker(120).name()
            else:
                if theme_background_color == "#FFFFFF":
                    tab_background_color = "#EEEEEE"
                else:
                    tab_background_color = QColor(theme_background_color).darker(110).name()

            tab_active_text_color = theme_foreground_color
            tab_inactive_text_color = QColor(tab_background_color).lighter(500).name()

            tab_active_background_color = theme_background_color
            tab_inactive_background_color = QColor(tab_background_color).lighter(150).name()

            tab_spliter_color = QColor(tab_background_color).lighter(180).name()

            [x, y, w, h] = emacs_frame_info
            painter.setPen(QColor(tab_background_color))
            painter.setBrush(QColor(tab_background_color))
            painter.drawRect(QRectF(x, y, w, sort_tab_info["tab_height"]))

        if "tab_names" in sort_tab_info and emacs_frame_info:
            tab_names = sort_tab_info["tab_names"]
            if len(tab_names) > 0:
                painter.setFont(self.font)
                metrics = QFontMetrics(self.font)

                tab_padding_x = 15

                current_tab_index = sort_tab_info["current_tab_index"]
                current_tab_x_offset = 0
                for index, tab_name in enumerate(tab_names):
                    tab_width = metrics.horizontalAdvance(tab_name)
                    if index >= current_tab_index:
                        [x, y, w, h] = emacs_frame_info
                        tab_right_edge_x = current_tab_x_offset + tab_width + tab_padding_x * 2
                        if tab_right_edge_x > w:
                            painter.translate(-(tab_right_edge_x // w * w - tab_width - tab_padding_x * 2), 0)
                        break
                    current_tab_x_offset += tab_width + tab_padding_x * 2

                tab_x_offset = 0
                for index, tab_name in enumerate(tab_names):
                    tab_width = metrics.horizontalAdvance(tab_name)

                    if index == sort_tab_info["current_tab_index"]:
                        painter.setPen(QColor(tab_active_background_color))
                        painter.setBrush(QColor(tab_active_background_color))
                    else:
                        painter.setPen(QColor(tab_inactive_background_color))
                        painter.setBrush(QColor(tab_inactive_background_color))

                    painter.drawRect(QRectF(tab_x_offset, 0, tab_width + tab_padding_x * 2, sort_tab_info["tab_height"]))

                    if index == sort_tab_info["current_tab_index"]:
                        painter.setPen(QColor(tab_active_text_color))
                        painter.setBrush(QColor(tab_active_text_color))
                    else:
                        painter.setPen(QColor(tab_inactive_text_color))
                        painter.setBrush(QColor(tab_inactive_text_color))

                    painter.drawText(QRectF(tab_x_offset, 0, tab_width + tab_padding_x * 2, sort_tab_info["tab_height"]),
                                     Qt.AlignmentFlag.AlignCenter,
                                     tab_name)

                    tab_x_offset += tab_width + tab_padding_x * 2

                tab_x_offset = 0
                for index, tab_name in enumerate(tab_names):
                    tab_width = metrics.horizontalAdvance(tab_name)
                    if index != len(tab_names) - 1:
                        painter.setPen(QColor(tab_spliter_color))
                        painter.setBrush(QColor(tab_spliter_color))
                        painter.drawRect(QRectF(tab_x_offset + tab_width + tab_padding_x * 2, 0, 1, sort_tab_info["tab_height"]))

                    tab_x_offset += tab_width + tab_padding_x * 2

        painter.restore()
