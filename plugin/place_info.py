import os
import re
import threading

from PyQt6.QtCore import QObject, QRectF, Qt
from PyQt6.QtGui import QColor, QFont, QFontDatabase, QFontMetrics, QPainterPath

from plugin.pystardict import Dictionary
from utils import *


class PlaceInfo(QObject):

    def __init__(self) -> None:
        super().__init__()

        self.words = {}
        self.margin = 20
        self.padding_horizontal = 20
        self.padding_vertical = 10

        [self.show_info,
         self.text_color,
         self.background_color,
         self.font_size,
         self.search_dictionary] = get_emacs_vars([
            "holo-layer-enable-place-info",
            "holo-layer-place-info-text-color",
            "holo-layer-place-info-background-color",
            "holo-layer-place-info-font-size",
            "holo-layer-place-info-dictionary"])

        self.font_family = QFontDatabase.systemFont(
            QFontDatabase.SystemFont.FixedFont
        ).family()
        self.font = QFont()
        self.font.setFamily(self.font_family)
        self.font.setPointSize(self.font_size)

        self.text_color = QColor(self.text_color)
        self.background_color = QColor(QColor(self.background_color).darker().name())
        self.background_color.setAlpha(220)

        if self.show_info:
            self.build_words_thread = threading.Thread(target=self.build_words)
            self.build_words_thread.start()

    def build_words(self):
        if self.search_dictionary == "kdic-ec-11w":
            dictionary_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), "resources", self.search_dictionary)
        else:
            dictionary_path = self.search_dictionary

        if os.path.exists("{}.ifo".format(dictionary_path)):
            start_dictionary = Dictionary(dictionary_path, in_memory=True)

            for word in start_dictionary.keys():
                first_line_translation = start_dictionary.dict[word].split()[0]
                no_phonetic_translation = first_line_translation.split(">")[-1]

                candidate_word  = word.lower().replace('\"', ' ')
                candidate_translation = no_phonetic_translation.strip().replace('\"', ' ')

                self.words[candidate_word] = candidate_translation
        else:
            message_emacs("StarDic dictionary {}.ifo is not exists".format(dictionary_path))

    def format_translation(self, translation):
        # Sort by word type.
        text = re.sub(r';(\w+\.)', r'\n\1', translation)

        # Filter long translation.
        text_lines = list(map(lambda line: self.short_translation_line(line), text.split("\n")))

        # Filter empty translation.
        text_lines = list(filter(lambda t: t != "", text_lines))

        # Join to multi-line content.
        text_content = "\n".join(text_lines)

        return (text_content, text_lines, len(text_lines))

    def short_translation_line(self, line):
        # We need remove some long translation avoid translation interfere with code content.
        words = line.split(";")
        short_words = list(filter(lambda w: len(w) < 15, words))
        return ";".join(short_words)

    def singular_word(self, word):
        try:
            import inflect

            engine = inflect.engine()
            return engine.singular_noun(word)
        except:
            return word

    def draw(self, painter, window_info, emacs_frame_info, word):
        if self.show_info and len(window_info) > 0:
            search_word = word if word in self.words else self.singular_word(word)

            if search_word and search_word in self.words:
                # Init rectangle vars.
                [x, y, w, h] = emacs_frame_info
                first_window_y = y + h

                # Calculate y coordinate of toppest window.
                for info in window_info:
                    if info[1] < first_window_y:
                        first_window_y = info[1]

                # Get multi-line translation.
                (text_content, text_lines, text_line_number) = self.format_translation(self.words[search_word])

                if len(text_lines) > 0:
                    # Set font.
                    painter.setFont(self.font)

                    # Set text and fill color.
                    painter.setPen(self.text_color)
                    painter.setBrush(self.background_color)

                    # Calculate translation width and height.
                    metrics = QFontMetrics(painter.font())
                    text_width = max(list(map(lambda t: metrics.horizontalAdvance(t), text_lines)))

                    # Calculate render rectangle.
                    text_rect = metrics.boundingRect(x + w - text_width - self.margin - self.padding_horizontal,
                                                     first_window_y + self.margin + self.padding_vertical,
                                                     text_width,
                                                     9999,  # some large height to accommodate the content
                                                     Qt.AlignmentFlag.AlignRight,
                                                     text_content)

                    background_rect = QRectF(text_rect)
                    background_rect = background_rect.adjusted(-self.padding_horizontal,
                                                               -self.padding_vertical,
                                                               self.padding_horizontal,
                                                               self.padding_vertical)

                    # Draw round rectangle.
                    path = QPainterPath()
                    roundness = 5
                    path.addRoundedRect(background_rect, roundness, roundness)
                    painter.fillPath(path, painter.brush())

                    # Draw translation.
                    painter.drawText(text_rect, Qt.AlignmentFlag.AlignRight, text_content)
