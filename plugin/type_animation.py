from PyQt6.QtCore import QPoint, QTimer, Qt, QPointF, QRectF
from PyQt6.QtGui import QPainter, QColor, QRadialGradient
from PyQt6.QtWidgets import QGraphicsView, QGraphicsScene, QGraphicsItem
import random
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

class FlameParticle(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        self.velocity = QPointF(random.uniform(-0.5, 0.5), random.uniform(-2, -1))
        self.opacity = 1.0
        self.size = random.uniform(3, 6)

    def boundingRect(self):
        return QRectF(-self.size/2, -self.size/2, self.size, self.size)

    def paint(self, painter, option, widget):
        gradient = QRadialGradient(QPointF(0, 0), self.size/2)
        gradient.setColorAt(0, self.color)
        gradient.setColorAt(1, Qt.GlobalColor.transparent)

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(gradient)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(self.boundingRect())

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.opacity -= 0.02
        self.size -= 0.1
        if self.opacity <= 0 or self.size <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class Flame(QGraphicsItem):
    def __init__(self, x, y):
        super().__init__()
        self.setPos(x, y)
        self.particles = []

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_particles)
        self.timer.start(10)

        QTimer.singleShot(400, self.remove_flame)

    def boundingRect(self):
        return QRectF(-20, -40, 40, 40)

    def paint(self, painter, option, widget):
        # Flame itself is invisible, only particles are visible
        pass

    def update_particles(self):
        self.particles = [p for p in self.particles if p.advance()]
        for _ in range(3):
            color = QColor(random.choice(['red', 'yellow', '#FFA500']))  # Using '#FFA500' for orange
            particle = FlameParticle(color)
            particle.setPos(random.uniform(-10, 10), 0)
            particle.setParentItem(self)
            self.particles.append(particle)

    def remove_flame(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)


class TypeAnimationScene(QGraphicsScene):
    def start_animation(self, x, y, style):
        if style == "firework":
            color = QColor(random.randint(0, 255), random.randint(0, 255), random.randint(0, 255))
            firework = Firework(x, y, color)
            self.addItem(firework)
        elif style == "flame":
            flame = Flame(x, y)
            self.addItem(flame)

class TypeAnimation(QGraphicsView):
    def __init__(self, parent=None):
        super().__init__(parent)

        self.firework_scene = TypeAnimationScene()
        self.setScene(self.firework_scene)
        self.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Make sure firework render coordinate same as cursor coordinate.
        self.setAlignment(Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft)

        (self.enable_type_animation, self.type_style) = get_emacs_vars([
            "holo-layer-enable-type-animation",
            "holo-layer-type-style"
        ])

    def trigger_firework(self, x, y):
        # It's important, we need call mapToScene to make sure firework at correct coordinate.
        point = QPoint(int(x), int(y))
        scene_pos = self.mapToScene(point)
        self.firework_scene.start_animation(scene_pos.x(), scene_pos.y(), self.type_style)
