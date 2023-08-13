import os
import re
import threading

from PyQt6.QtCore import QObject, QRect, Qt
from PyQt6.QtGui import QColor, QFont, QFontMetrics

from plugin.pystardict import Dictionary
from utils import *


class PlaceInfo(QObject):

    def __init__(self) -> None:
        super().__init__()

        self.words = {}
        self.margin = 20
        self.padding = 10

        [self.search_dictionary] = get_emacs_vars(["holo-layer-place-info-dictionary"])

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

    def draw(self, painter, window_info, emacs_frame_info, word):
        if len(window_info) > 0:
            if word in self.words:
                font = QFont()
                font.setPointSize(20)
                painter.setFont(font)

                painter.setPen(QColor(0, 255, 0))
                painter.setBrush(QColor(0, 0, 0, 50))

                [x, y, w, h] = emacs_frame_info
                first_window_y = y + h

                for info in window_info:
                    if info[1] < first_window_y:
                        first_window_y = info[1]

                metrics = QFontMetrics(painter.font())
                text = re.sub(r';(\w+\.)', r'\n\1', self.words[word])
                text_width = metrics.horizontalAdvance(text)
                text_height = metrics.height()

                background_rect = QRect(x + w - text_width - self.margin - self.padding * 2,
                                        first_window_y + self.margin,
                                        text_width + self.padding * 2,
                                        text_height)
                painter.fillRect(background_rect, painter.brush())

                text_rect = QRect(x + w - text_width - self.margin - self.padding,
                                  first_window_y + self.margin,
                                  text_width,
                                  text_height)
                painter.drawText(text_rect,
                                 Qt.AlignmentFlag.AlignRight,
                                 re.sub(r';(\w+\.)', r'\n\1', self.words[word]))

