import numpy as np
from matplotlib import pyplot as plt
from utils.image import convert_to_opencv_format, convert_to_tensor_format
import cv2


def show_image(image, show_immediately=True, title=None):
    is_grey = image.shape[0] == 1
    image = convert_to_opencv_format(image, convert_color=False)
    if title is not None:
        plt.title(title)
    cmap = 'gray' if is_grey else None
    plt.imshow(image, cmap=cmap)
    if show_immediately:
        plt.show()


class ImageDrawer:
    @staticmethod
    def draw_rect(image, rect, rgb=(255, 0, 0), thickness=1):
        image_to_draw = ImageDrawer._prepare_for_drawing(image)
        cv2.rectangle(image_to_draw, rect[0], rect[1], rgb[::-1], thickness)
        return convert_to_tensor_format(image_to_draw)

    @staticmethod
    def draw_poly(image, points, is_closed=True, rgb=(255, 0, 0), thickness=1):
        image_to_draw = ImageDrawer._prepare_for_drawing(image)
        points = np.array(points, np.int).reshape(-1, 1, 2)
        cv2.polylines(image_to_draw, [points], is_closed, rgb[::-1], thickness)
        return convert_to_tensor_format(image_to_draw)

    @staticmethod
    def _prepare_for_drawing(image):
        is_grey = image.shape[0] == 1
        image_to_draw = convert_to_opencv_format(image)
        if is_grey:
            image_to_draw = cv2.cvtColor(image_to_draw, cv2.COLOR_GRAY2BGR)
        return image_to_draw
