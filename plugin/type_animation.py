from PyQt6.QtCore import QPoint, QTimer
from PyQt6.QtGui import QPainter, QColor, QLinearGradient
from PyQt6.QtWidgets import QGraphicsView, QGraphicsScene
from PyQt6.QtCore import Qt

import random
from PyQt6.QtWidgets import QGraphicsItem
from PyQt6.QtCore import QPointF, QRectF

from utils import *

class Particle(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        self.velocity = QPointF(random.uniform(-1, 1), random.uniform(-1, 1))
        self.opacity = 1.0

    def boundingRect(self):
        return QRectF(-1.5, -1.5, 3, 3)

    def paint(self, painter, option, widget):
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(self.color)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(self.boundingRect())

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.opacity -= 0.02
        if self.opacity <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class Firework(QGraphicsItem):
    def __init__(self, x, y, color):
        super().__init__()
        self.size = 20

        self.setPos(x, y + self.size * 2)

        self.particles = [Particle(color) for _ in range(30)]
        for particle in self.particles:
            particle.setParentItem(self)

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_particles)
        self.timer.start(10)

        QTimer.singleShot(2000, self.remove_firework)

    def boundingRect(self):
        return QRectF(-self.size, -self.size, self.size * 2, self.size * 2)

    def update_particles(self):
        self.particles = [p for p in self.particles if p.advance()]
        if not self.particles:
            self.remove_firework()

    def remove_firework(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)

class TypeAnimationScene(QGraphicsScene):
    def fire(self, x, y):
        color = QColor(random.randint(0, 255), random.randint(0, 255), random.randint(0, 255))
        firework = Firework(x, y, color)
        self.addItem(firework)

class TypeAnimation(QGraphicsView):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.firework_scene = TypeAnimationScene()
        self.setScene(self.firework_scene)
        self.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Make sure firework render coordinate same as cursor coordinate.
        self.setAlignment(Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft)

        self.enable_cursor_firework = get_emacs_var("holo-layer-enable-type-animation")

    def trigger_firework(self, x, y):
        # It's important, we need call mapToScene to make sure firework at correct coordinate.
        point = QPoint(int(x), int(y))
        scene_pos = self.mapToScene(point)
        self.firework_scene.fire(scene_pos.x(), scene_pos.y())

    # def drawBackground(self, painter, rect):
    #     gradient = QLinearGradient(rect.topLeft(), rect.bottomLeft())
    #     gradient.setColorAt(0, QColor(0, 0, 50, 125))
    #     gradient.setColorAt(1, QColor(25, 25, 112, 125))

    #     painter.fillRect(rect, gradient)
