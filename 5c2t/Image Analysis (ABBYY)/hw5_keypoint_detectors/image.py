import cv2


def read_image(path, mode=cv2.IMREAD_GRAYSCALE):
    image = cv2.imread(path, mode)
    if image is None:
        raise IOError('Cannot read image by path: ' + path)
    return image
