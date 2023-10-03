from PyQt6 import QtCore
from PyQt6.QtCore import QObject, Qt, QRectF, QMimeDatabase
from PyQt6.QtGui import QColor, QFontMetrics, QFontDatabase, QFont, QIcon, QRegion

from utils import *

import os

# File extension to MIME lookup table
FILE_SUFFIX_MIME_DICT = {
    "json": "application-json",
    "vue": "application-javascript"
}

# Mode to icon lookup table
MODE_ICON_DICT = {
    "eaf-file-manager": "directory",
    "eaf-browser": "browser",
    "eaf-git": "git",
    "eaf-map": "map",
    "eaf-rss-reader": "rss-reader",
    "eaf-music-player": "music-player",
    "eaf-pyqterminal": "terminal",
    "eaf-terminal": "terminal",
    "eaf-camera": "camera"
}

class SortTab(QObject):

    def __init__(self) -> None:
        super().__init__()

        # Set font.
        [self.font_size] = get_emacs_vars([
            "holo-layer-sort-tab-font-size"
        ])
        self.font_family = QFontDatabase.systemFont(
            QFontDatabase.SystemFont.FixedFont # get the system's monospaced font
        ).family()
        self.font = QFont()
        self.font.setFamily(self.font_family)
        self.font.setPointSize(self.font_size)

        # Set some variables.
        self.tab_scroll_pos = 0 # scroll position, records the horizontal scroll position of the tab bar
        self.tab_icon_size = 22 # tab icon size
        self.tab_padding_x = 15 # padding around tab text
        self.tab_icon_padding_right = 10 # padding between tab icon and tab text
        self.tab_translate_offset = 60 # translate offset, ensure that users can see whether there are other tabs around current tab

        # TODO
        # We need install WhiteSur icon theme now.
        # We need remove below code after icon cache is enough.
        if os.path.exists("/usr/share/icons/WhiteSur"):
            QIcon.setThemeName("WhiteSur")
        else:
            message_emacs("You need install icon theme `WhiteSur` to show icon in sort-tab.")

        # Create icon cache directory.
        self.icon_cache_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), "icon_cache")
        if not os.path.exists(self.icon_cache_dir):
            os.makedirs(self.icon_cache_dir)

        # Create mime database.
        self.mime_db = QMimeDatabase()

    def draw(self, painter, emacs_frame_info, sort_tab_info):
        # Save painter to restore at last of draw function.
        painter.save()

        # Set clip to entire emacs area, allow to render sort-tab.
        if emacs_frame_info:
            [x, y, w, h] = emacs_frame_info
            painter.setClipRegion(QRegion(x, y, w, h))

        # Draw tab line background.
        if "emacs_theme_mode" in sort_tab_info and emacs_frame_info:
            # Get emacs theme colors.
            theme_mode = sort_tab_info["emacs_theme_mode"]
            theme_foreground_color = sort_tab_info["emacs_theme_foreground_color"]
            theme_background_color = sort_tab_info["emacs_theme_background_color"]

            # Create tab background color base on emacs background color.
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

            # Create tab colors.
            tab_active_text_color = theme_foreground_color
            tab_inactive_text_color = QColor(tab_background_color).lighter(500).name()
            tab_active_background_color = theme_background_color
            tab_inactive_background_color = QColor(tab_background_color).lighter(150).name()
            tab_spliter_color = QColor(tab_background_color).lighter(180).name()

            # Draw tab background.
            [x, y, w, h] = emacs_frame_info
            painter.setPen(QColor(tab_background_color))
            painter.setBrush(QColor(tab_background_color))
            painter.drawRect(QRectF(x, y, w, sort_tab_info["tab_height"]))

        if "tab_names" in sort_tab_info and emacs_frame_info:
            tab_names = sort_tab_info["tab_names"]

            if len(tab_names) > 0:
                # Set font.
                painter.setFont(self.font)
                metrics = QFontMetrics(self.font)

                # Translate tab line along with scroll position.
                current_tab_index = sort_tab_info["current_tab_index"]
                current_tab_x_offset = 0
                for index, tab_name in enumerate(tab_names):
                    # Calculate tab width and icon offset.
                    tab_width = metrics.horizontalAdvance(tab_name)
                    (icon_path, icon_offset) = self.get_tab_icon_info(tab_name, sort_tab_info["tab_modes"][index])

                    # Calculable tab edge offfset.
                    tab_left_edge_x = current_tab_x_offset
                    tab_right_edge_x = current_tab_x_offset + icon_offset + tab_width + self.tab_padding_x * 2

                    # Only translate when touch current index.
                    if index >= current_tab_index:
                        [x, y, w, h] = emacs_frame_info

                        if tab_left_edge_x < self.tab_scroll_pos:
                            # Make sure tab in visible area if it out of screen left edge.
                            self.tab_scroll_pos = max(tab_left_edge_x - self.tab_translate_offset, 0)
                        elif tab_right_edge_x > self.tab_scroll_pos + w:
                            # Make sure tab in visible area if it out of screen right edge.
                            self.tab_scroll_pos = tab_right_edge_x - w + self.tab_translate_offset

                        # Painter translate direction is a minus number.
                        painter.translate(-self.tab_scroll_pos, 0)

                        # Jump out loop after painter translate.
                        break

                    # Calculable tab render offset base on left tabs.
                    current_tab_x_offset += icon_offset + tab_width + self.tab_padding_x * 2

                # Draw tabs.
                tab_x_offset = 0
                for index, tab_name in enumerate(tab_names):
                    # Get tab width and icon offset.
                    tab_width = metrics.horizontalAdvance(tab_name)
                    (icon_path, icon_offset) = self.get_tab_icon_info(tab_name, sort_tab_info["tab_modes"][index])

                    # Draw tab background.
                    if index == sort_tab_info["current_tab_index"]:
                        painter.setPen(QColor(tab_active_background_color))
                        painter.setBrush(QColor(tab_active_background_color))
                    else:
                        painter.setPen(QColor(tab_inactive_background_color))
                        painter.setBrush(QColor(tab_inactive_background_color))

                    painter.drawRect(QRectF(tab_x_offset, 0, icon_offset + tab_width + self.tab_padding_x * 2, sort_tab_info["tab_height"]))

                    # Draw tab icon if tab has icon.
                    if os.path.exists(icon_path):
                        icon = QIcon(icon_path)
                        pixmap = icon.pixmap(self.tab_icon_size, self.tab_icon_size)
                        icon_padding_y = 2
                        painter.drawPixmap(tab_x_offset + self.tab_padding_x,
                                           int((sort_tab_info["tab_height"] - self.tab_icon_size) / 2) + icon_padding_y,
                                           pixmap)

                    # Draw tab text.
                    if index == sort_tab_info["current_tab_index"]:
                        painter.setPen(QColor(tab_active_text_color))
                        painter.setBrush(QColor(tab_active_text_color))
                    else:
                        painter.setPen(QColor(tab_inactive_text_color))
                        painter.setBrush(QColor(tab_inactive_text_color))

                    painter.drawText(QRectF(tab_x_offset + icon_offset, 0, tab_width + self.tab_padding_x * 2, sort_tab_info["tab_height"]),
                                     Qt.AlignmentFlag.AlignCenter,
                                     tab_name)

                    # Calculable tab render offset base on left tabs.
                    tab_x_offset += icon_offset + tab_width + self.tab_padding_x * 2

                # Draw tab spliter, we can't draw tab spliter along with tab content, otherwhere tab spliter will override by next tab content.
                tab_x_offset = 0
                for index, tab_name in enumerate(tab_names):
                    # Get tab width and icon offset.
                    tab_width = metrics.horizontalAdvance(tab_name)
                    (icon_path, icon_offset) = self.get_tab_icon_info(tab_name, sort_tab_info["tab_modes"][index])

                    # Draw tab spliter.
                    if index != len(tab_names) - 1:
                        painter.setPen(QColor(tab_spliter_color))
                        painter.setBrush(QColor(tab_spliter_color))
                        painter.drawRect(QRectF(tab_x_offset + icon_offset + tab_width + self.tab_padding_x * 2, 0, 1, sort_tab_info["tab_height"]))

                    # Calculable tab render offset base on left tabs.
                    tab_x_offset += icon_offset + tab_width + self.tab_padding_x * 2

        # Restore painter.
        painter.restore()

    def get_tab_icon_info(self, tab_name, mode_name):
        # Get file info.
        file_info = QtCore.QFileInfo(tab_name)
        file_suffix = file_info.suffix()

        # Calculable mime type base on buffer mode or file suffix.
        if mode_name in MODE_ICON_DICT:
            mime = MODE_ICON_DICT[mode_name]
        elif file_suffix in FILE_SUFFIX_MIME_DICT:
            mime = FILE_SUFFIX_MIME_DICT[file_suffix]
        else:
            mime = self.mime_db.mimeTypeForFile(file_info).name().replace("/", "-")

        # Build icon variables.
        icon_name = "{}.{}".format(mime, "png")
        icon_path = os.path.join(self.icon_cache_dir, icon_name)

        # Try to fetch icon from system theme if icon cache not exists.
        # Icon in system theme is small, not clear, suggest download icon from https://icons8.com/icons/set
        if not os.path.exists(icon_path):
            icon = QIcon.fromTheme(mime, QIcon("text-plain"))

            # If nothing match, icon size is empty.
            # Then we use fallback icon.
            if icon.availableSizes() == []:
                icon = QIcon.fromTheme("text-plain")

            icon.pixmap(64, 64).save(icon_path)

        # Debug code, remove it after icon cache is enough.
        print("***** ", tab_name, mode_name, mime)

        # Return icon info.
        if os.path.exists(icon_path):
            icon_offset = self.tab_icon_size + self.tab_icon_padding_right
            return (icon_path, icon_offset)
        else:
            return (None, 0)
