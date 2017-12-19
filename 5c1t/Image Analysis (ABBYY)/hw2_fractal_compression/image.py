import cv2


def read_image(path, *args, **kwargs):
    image = cv2.imread(path, *args, **kwargs)
    if image is None:
        raise ValueError(f'Cannot read image: {path}')
    return image


def save_image(image, path):
    cv2.imwrite(path, image)
