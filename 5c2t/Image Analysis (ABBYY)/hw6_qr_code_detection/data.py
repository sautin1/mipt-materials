import json
from os.path import basename, dirname, splitext, join, exists

from utils.io import search_for_extensions, read_image
from utils.paths import PATH_DATA


PATH_SOEROES_DATASET = join(PATH_DATA, 'soeroesg_iPhone4S_QR')
PATH_DUBSKA_QRCODE_DATASET = join(PATH_DATA, 'dubska-qrcode-datasets', 'datasets')
PATH_DUBSKA_MATRIX_DATASET = join(PATH_DATA, 'dubska-matrix-dataset')
PATH_ABBYY_DATASET = join(PATH_DATA, 'abbyy')
PATH_BARCODES_DATASET = join(PATH_DATA, 'barcodes')


class DatasetReader:
    def __init__(self, path, extensions=None):
        extensions = extensions or {'.png', '.jpg'}
        self._paths, self._markup = self._load_markup(search_for_extensions(path, extensions))

    def _load_markup(self, paths):
        image_id_to_markup = {}
        paths_labelled = []
        for path_image in paths:
            image_id = self.get_image_id(path_image)
            path_markup = splitext(path_image)[0] + '_markup.json'
            if not exists(path_markup):
                continue
            paths_labelled.append(path_image)
            with open(path_markup, 'r') as fin:
                image_id_to_markup[image_id] = json.load(fp=fin)
        return paths_labelled, image_id_to_markup

    def get_paths(self):
        return self._paths

    def get_images_with_ids(self):
        return ((read_image(path), self.get_image_id(path)) for path in self._paths)

    def get_image_id(self, path):
        return splitext(join(basename(dirname(path)), basename(path)))[0]

    def get_markup(self):
        return self._markup


if __name__ == '__main__':
    from utils.visualization import show_image, ImageDrawer

    dataset = DatasetReader(PATH_ABBYY_DATASET)
    image, image_id = next(dataset.get_images_with_ids())
    print(image_id)
    print(image.shape)
    markup = dataset.get_markup()[image_id]
    print(markup)
    image_with_poly = image
    for corner in markup:
        image_with_poly = ImageDrawer.draw_poly(image_with_poly, corner['pts'], rgb=(0, 255, 0), thickness=4)
    show_image(image_with_poly)
