import cv2
import os
from os.path import join

from utils.image import convert_to_tensor_format, convert_to_opencv_format


def read_image(path, is_grey=False):
    image = cv2.imread(path, cv2.IMREAD_COLOR if not is_grey else cv2.IMREAD_GRAYSCALE)
    if image is None:
        raise IOError(f'Cannot read image: {path}')
    return convert_to_tensor_format(image)


def save_image(image, path):
    image = convert_to_opencv_format(image)
    cv2.imwrite(path, image)


def search_for_extensions(path_root, extensions=None):
    for root, dirs, files in os.walk(path_root):
        for filename in files:
            extension = os.path.splitext(filename)[1]
            if extensions is None or extension.lower() in extensions:
                yield join(root, filename)


if __name__ == '__main__':
    from os.path import join
    from utils.paths import PATH_DATA
    from utils.visualization import show_image

    path = join(PATH_DATA, 'dubska-qrcode-datasets', 'bitmaps', 'brown.png')
    image = read_image(path, is_grey=True)
    show_image(image)
    print(image.shape)
    print(image.min(), image.mean(), image.max())
