import numpy as np
import cv2
import csv
from os.path import basename, dirname, splitext, join

from utils.io import search_for_extensions, read_image
from utils.geometry import calc_bounding_box
from utils.paths import PATH_DATA


PATH_SOEROES_DATASET = join(PATH_DATA, 'soeroesg_iPhone4S_QR')
PATH_DUBSKA_QRCODE_DATASET = join(PATH_DATA, 'dubska-qrcode-datasets', 'datasets')
PATH_ABBYY_DATASET = join(PATH_DATA, 'abbyy')
PATH_BARCODES_DATASET = join(PATH_DATA, 'barcodes')


class SoeroesDataset:
    def __init__(self, path=PATH_SOEROES_DATASET):
        self._paths = list(search_for_extensions(path, {'.png'}))
        self._markup = self._load_markup(self._paths)

    def _load_markup(self, paths):
        image_id_to_markup = {}
        for path_image in paths:
            image_id = self.get_image_id(path_image)
            path_markup = splitext(path_image)[0] + '_pts.yml'
            fs = cv2.FileStorage(path_markup, cv2.FILE_STORAGE_READ)
            markup_x = fs.getNode('x').mat()
            markup_y = fs.getNode('y').mat()
            markup = np.concatenate((markup_x, markup_y), 1)
            fs.release()
            image_id_to_markup[image_id] = markup
        return image_id_to_markup

    def get_images_with_ids(self):
        return ((read_image(path), self.get_image_id(path)) for path in self._paths)

    def get_image_id(self, path):
        return splitext(basename(path))[0]

    def get_markup(self):
        return self._markup

    def get_bounding_boxes(self):
        return {image_id: [calc_bounding_box(points)]
                for image_id, points in self._markup.items()}


class DubskaQrCodeDataset:
    def __init__(self, path=PATH_DUBSKA_QRCODE_DATASET):
        self._paths = list(search_for_extensions(path, {'.jpg'}))
        self._markup = self._load_markup(self._paths)

    def _load_markup(self, paths):
        image_id_to_markup = {}
        fields = {'x': int, 'y': int, 'radius': int, 'rotate': bool, 'blur': bool, 'project': bool, 'lighting': bool}
        for path_image in paths:
            image_id = self.get_image_id(path_image)
            path_markup = splitext(path_image)[0] + '_annotated.csv'
            with open(path_markup, 'r') as fin:
                reader = csv.DictReader(fin, fieldnames=fields.keys())
                next(reader)
                image_id_to_markup[image_id] = [{key_name: key_type(row[key_name])
                                                 for key_name, key_type in fields.items()} for row in reader]
        return image_id_to_markup

    def get_images_with_ids(self):
        return ((read_image(path), self.get_image_id(path)) for path in self._paths)

    def get_image_id(self, path):
        return splitext(join(basename(dirname(path)), basename(path)))[0]

    def get_markup(self):
        return self._markup

    def get_bounding_boxes(self):
        return {image_id: [((markup['x'] - markup['radius'],
                            markup['y'] - markup['radius']),
                           (markup['x'] + markup['radius'],
                            markup['y'] + markup['radius'])) for markup in markups]
                for image_id, markups in self._markup.items()}


if __name__ == '__main__':
    from utils.visualization import show_image, ImageDrawer
    from utils.geometry import clip_bounding_box

    dataset = DubskaQrCodeDataset()
    image, image_id = next(dataset.get_images_with_ids())
    print(image_id)
    print(image.shape)
    print(dataset.get_markup()[image_id])
    boxes = dataset.get_bounding_boxes()[image_id]
    print(boxes)
    image_with_rect = ImageDrawer.draw_rect(image, clip_bounding_box(boxes[0], image.shape), thickness=4)
    show_image(image_with_rect)
