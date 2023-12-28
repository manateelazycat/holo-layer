from PyQt6.QtCore import QObject
from PyQt6.QtGui import QColor

from utils import *


class IndentLine(QObject):

    def __init__(self) -> None:
        super().__init__()

        self.rainbow_indent_colors = list(map(lambda x: QColor(x), get_emacs_var("holo-layer-indent-colors")))

    def draw(self, painter, emacs_indent_infos, emacs_frame_info):
        if emacs_indent_infos and emacs_frame_info:
            [emacs_x, emacs_y, _, _] = emacs_frame_info
            for window_indent_info in emacs_indent_infos:
                if '_' not in window_indent_info or ':' not in window_indent_info:
                    continue

                window_info, cursor_info, indents = window_indent_info.split('_')
                # TODO only line indent is not enough to get indent line
                # need add text objects info from lang parser
                indents = [int(i) for i in indents.split(',')]
                cursor_x, cursor_y, cursor_w, cursor_h = [int(i) for i in cursor_info.split(':')]
                window_info = [int(i) for i in window_indent_info.split(':')[:4]]

                x, y, w, h = window_info
                x = cursor_x + emacs_x
                y = cursor_y + emacs_y

                # Add some offset make sure indent at right of indent char.
                x += 5

                ava_indents = sorted(set(indents))
                if ava_indents[0] == 0:
                    del ava_indents[0]

                del_index = []
                # TODO get indent level from emacs
                # and skip indent that not multiple of index level
                for ci, indent_level in enumerate(ava_indents):
                    if indent_level % 2 != 0:
                        del_index.append(ci)
                for index in sorted(del_index, reverse=True):
                    del ava_indents[index]

                # first sort available indents by indent level
                # add indent line according to indent level
                for ci, indent_level in enumerate(ava_indents):
                    # Don't draw first indent line at column 0.
                    if ci == 0 and indent_level == 0:
                        continue

                    painter.setPen(self.rainbow_indent_colors[ci % len(self.rainbow_indent_colors)])
                    last_index = -1
                    for index, indent in enumerate(indents):
                        if indent > indent_level or indent == -1:
                            continue
                        cur_indents = indents[last_index+1:index]
                        if len(cur_indents) > 0 and \
                           max(cur_indents) > indent_level and sum(cur_indents) > 0:
                            # TODO cache draw states
                            painter.drawLine(x + indent_level * cursor_w, y + cursor_h * (last_index + 1),
                                             x + indent_level * cursor_w, y + min(h, cursor_h * index))
                        last_index = index

                    if last_index < len(indents) - 1:
                        painter.drawLine(x + indent_level * cursor_w, y + cursor_h * (last_index + 1),
                                         x + indent_level * cursor_w, y + min(h, cursor_h * len(indents)))
