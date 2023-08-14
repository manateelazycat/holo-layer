from PyQt6.QtCore import QObject, pyqtSignal
from PyQt6.QtGui import QGuiApplication

from utils import message_emacs


class WindowScreenshot(QObject):

    update_animation = pyqtSignal()

    def __init__(self) -> None:
        super().__init__()

    def take_screenshot(self, window_info, emacs_frame_info):
        [x, y, w, h] = list(map(int, window_info.split(":")[:4]))
        self.window_info = [int(x), int(y), int(w), int(h)]
        [emacs_x, emacs_y, emacs_width, emacs_height] = emacs_frame_info
        self.emacs_frame_info = emacs_frame_info

        screen = QGuiApplication.primaryScreen()

        screenshot = screen.grabWindow(0, x + emacs_x, y + emacs_y, w, h)
        screenshot_path = "/home/andy/screenshot.png"
        screenshot.save(screenshot_path, 'png')

        message_emacs(f"Save screenshot to {screenshot_path}.")

    def draw(self, painter):
        pass
