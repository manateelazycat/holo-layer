from PyQt6.QtGui import QColor, QPen
from PyQt6.QtCore import QPoint, QTimer, Qt, QPointF, QRectF
from PyQt6.QtGui import QPainter, QRadialGradient
from PyQt6.QtWidgets import QGraphicsView, QGraphicsScene, QGraphicsItem
from PyQt6.QtGui import QPixmap
import math
import random
from utils import *

class WaterDroplet(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        self.size = random.uniform(2, 6)
        angle = random.uniform(0, 2 * math.pi)
        speed = random.uniform(1, 3)
        self.velocity = QPointF(speed * math.cos(angle), speed * math.sin(angle) - 2)
        self.opacity = 1.0

    def boundingRect(self):
        return QRectF(-self.size/2, -self.size/2, self.size, self.size)

    def paint(self, painter, option, widget):
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(self.color)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(self.boundingRect())

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.velocity += QPointF(0, 0.1)  # Gravity effect
        self.opacity -= 0.02
        self.size -= 0.05
        if self.opacity <= 0 or self.size <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class WaterSplash(QGraphicsItem):
    def __init__(self, x, y):
        super().__init__()
        self.setPos(x, y)
        self.particles = []
        self.timer = QTimer()
        self.timer.timeout.connect(self.update_particles)
        self.timer.start(16)  # 60 FPS

        self.create_particles()
        QTimer.singleShot(1000, self.remove_splash)

    def boundingRect(self):
        return QRectF(-50, -50, 100, 100)

    def paint(self, painter, option, widget):
        # The splash itself is invisible, only particles are visible
        pass

    def create_particles(self):
        for _ in range(20):
            color = QColor(random.choice(['#4FC3F7', '#29B6F6', '#03A9F4', '#039BE5', '#0288D1']))
            particle = WaterDroplet(color)
            particle.setParentItem(self)
            self.particles.append(particle)

    def update_particles(self):
        self.particles = [p for p in self.particles if p.advance()]

    def remove_splash(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)

class BalloonParticle(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        self.velocity = QPointF(random.uniform(-0.5, 0.5), random.uniform(-2, -1))
        self.opacity = 1.0
        self.size = random.uniform(10, 20)
        self.string_opacity = 1.0
        self.string_length = self.size * 1.5  # 增加线的长度

    def boundingRect(self):
        return QRectF(-self.size/2, -self.size/2, self.size, self.size + self.string_length)

    def paint(self, painter, option, widget):
        # 绘制气球
        painter.setPen(QPen(self.color.darker(), 1))
        painter.setBrush(self.color)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(QRectF(-self.size/2, -self.size/2, self.size, self.size))

        # 绘制气球线
        painter.setPen(QPen(Qt.GlobalColor.black, 1))
        painter.setOpacity(self.string_opacity)
        painter.drawLine(QPointF(0, self.size/2), QPointF(0, self.size/2 + self.string_length))

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.opacity -= 0.01
        self.string_opacity -= 0.015  # 线比气球消失得稍快
        if self.opacity <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class Balloon(QGraphicsItem):
    def __init__(self, x, y):
        super().__init__()
        self.setPos(x, y)
        self.particles = []

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_particles)
        self.timer.start(10)

        QTimer.singleShot(1000, self.remove_balloon)

    def boundingRect(self):
        return QRectF(-30, -60, 60, 60)

    def paint(self, painter, option, widget):
        # Balloon itself is invisible, only particles are visible
        pass

    def update_particles(self):
        self.particles = [p for p in self.particles if p.advance()]
        if len(self.particles) < 5:
            color = QColor(random.choice([
                '#FF6B6B',  # 珊瑚红
                '#4ECDC4',  # 薄荷绿
                '#45B7D1',  # 天蓝色
                '#FFA07A',  # 浅鲑鱼色
                '#98D8C8',  # 海泡色
                '#F7E8A6',  # 香槟色
                '#AED9E0',  # 粉蓝色
                '#FAD02E',  # 向日葵黄
                '#B19CD9',  # 淡紫色
                '#FF90B3',  # 粉红色
                '#7FBC8C',  # 鼠尾草绿
                '#C5E99B',  # 嫩绿色
                '#F9D5E5',  # 浅粉色
                '#E6E6FA',  # 薰衣草色
                '#FFD700',  # 金色
            ]))

            particle = BalloonParticle(color)
            particle.setPos(random.uniform(-20, 20), random.uniform(-20, 20))
            particle.setParentItem(self)
            self.particles.append(particle)

    def remove_balloon(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)


class LightningBolt(QGraphicsItem):
    def __init__(self, start, end, branch_probability=0.3):
        super().__init__()
        self.start = start
        self.end = end
        self.branch_probability = branch_probability
        self.segments = []
        self.generate_lightning()
        self.opacity = 1.0

        self.pixmap = None
        self.bounding_rect = None
        self.create_pixmap()

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_lightning)
        self.timer.start(10)

    def generate_lightning(self):
        self.segments = []
        self.create_segment(self.start, self.end)

    def create_segment(self, start, end):
        if (end - start).manhattanLength() < 10:
            self.segments.append((start, end))
            return

        mid = QPointF((start.x() + end.x()) / 2, (start.y() + end.y()) / 2)
        displacement = QPointF(-(end.y() - start.y()), end.x() - start.x())
        displacement *= (random.random() * 0.4 - 0.2)
        mid += displacement

        self.create_segment(start, mid)
        self.create_segment(mid, end)

        if random.random() < self.branch_probability:
            branch_end = mid + (mid - start) * 0.7
            branch_end += QPointF(random.random() * 20 - 10, random.random() * 20 - 10)
            self.create_segment(mid, branch_end)

    def boundingRect(self):
        return self.bounding_rect

    def create_pixmap(self):
        rect = QRectF(self.start, self.end).normalized().adjusted(-20, -20, 20, 20)
        self.bounding_rect = rect
        self.pixmap = QPixmap(rect.size().toSize())
        self.pixmap.fill(Qt.GlobalColor.transparent)

        painter = QPainter(self.pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        painter.translate(-rect.topLeft())

        main_color = QColor(200, 220, 255, 255)
        painter.setPen(QPen(main_color, 3, Qt.PenStyle.SolidLine, Qt.PenCapStyle.RoundCap, Qt.PenJoinStyle.RoundJoin))
        for start, end in self.segments:
            painter.drawLine(start, end)

        glow_color = QColor(220, 240, 255, 100)
        painter.setPen(QPen(glow_color, 6, Qt.PenStyle.SolidLine, Qt.PenCapStyle.RoundCap, Qt.PenJoinStyle.RoundJoin))
        for start, end in self.segments:
            painter.drawLine(start, end)

        painter.end()

    def paint(self, painter, option, widget):
        painter.setOpacity(self.opacity)
        painter.drawPixmap(self.boundingRect().topLeft(), self.pixmap)

    def update_lightning(self):
        self.opacity -= 0.05
        if self.opacity <= 0:
            if self.scene():
                self.scene().removeItem(self)
            self.timer.stop()
        self.update()

class SupernovaParticle(QGraphicsItem):
    def __init__(self, color):
        super().__init__()
        self.color = color
        angle = random.uniform(0, 2 * 3.14159)
        speed = random.uniform(1, 3)
        self.velocity = QPointF(speed * math.cos(angle), speed * math.sin(angle))
        self.opacity = 1.0
        self.size = 4

    def boundingRect(self):
        return QRectF(-self.size/2, -self.size/2, self.size, self.size)

    def paint(self, painter, option, widget):
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(self.color)
        painter.setOpacity(self.opacity)
        painter.drawEllipse(self.boundingRect())

    def advance(self):
        self.setPos(self.pos() + self.velocity)
        self.opacity -= 0.02
        self.size -= 0.05
        if self.opacity <= 0 or self.size <= 0:
            if self.scene():
                self.scene().removeItem(self)
            return False
        return True

class Supernova(QGraphicsItem):
    def __init__(self, x, y):
        super().__init__()
        self.particles = []
        self.core_size = 30
        self.core_opacity = 1.0

        self.setPos(x, y - 50)

        self.timer = QTimer()
        self.timer.timeout.connect(self.update_supernova)
        self.timer.start(10)

        QTimer.singleShot(300, self.remove_supernova)

    def boundingRect(self):
        return QRectF(-100, -100, 200, 200)

    def update_supernova(self):
        # Update existing particles
        self.particles = [p for p in self.particles if p.advance()]

        # Create new particles
        for _ in range(5):
            color = QColor(random.choice(['red', 'yellow', 'orange', 'white']))
            particle = SupernovaParticle(color)
            particle.setPos(0, 0)
            particle.setParentItem(self)
            self.particles.append(particle)

        # Update core
        self.core_size += 0.5
        self.core_opacity -= 0.01
        if self.core_opacity < 0:
            self.core_opacity = 0

        self.update()

    def remove_supernova(self):
        scene = self.scene()
        if scene:
            scene.removeItem(self)

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
        elif style == "water":
            splash = WaterSplash(x, y)
            self.addItem(splash)
        elif style == "flame":
            flame = Flame(x, y)
            self.addItem(flame)
        elif style == "balloon":
            balloon = Balloon(x, y)
            self.addItem(balloon)
        elif style == "lightning":
            angle = random.uniform(0, 2 * math.pi)
            length = random.uniform(100, 200)
            end_x = x + length * math.cos(angle)
            end_y = y + length * math.sin(angle)
            lightning = LightningBolt(QPointF(x, y), QPointF(end_x, end_y))
            self.addItem(lightning)
        elif style == "supernova":
            supernova = Supernova(x, y)
            self.addItem(supernova)

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
            "holo-layer-type-animation-style"
        ])

    def trigger_firework(self, x, y):
        # It's important, we need call mapToScene to make sure firework at correct coordinate.
        point = QPoint(int(x), int(y))
        scene_pos = self.mapToScene(point)
        self.firework_scene.start_animation(scene_pos.x(), scene_pos.y(), self.type_style)
