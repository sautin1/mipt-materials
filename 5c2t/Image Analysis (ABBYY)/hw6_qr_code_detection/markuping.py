from tqdm import tqdm
import subprocess
import shlex
import json
from os.path import join, splitext
from scipy.spatial import ConvexHull
import numpy as np

from os import listdir, environ
from data import PATH_DUBSKA_QRCODE_DATASET, PATH_SOEROES_DATASET, PATH_ABBYY_DATASET, PATH_BARCODES_DATASET


def get_corner_points(points):
    hull = ConvexHull(np.array(points))
    return [points[idx] for idx in hull.vertices]


if __name__ == '__main__':
    path_root = PATH_BARCODES_DATASET
    bash_command = './qr_image \'{}\''
    for dir_name in listdir(path_root):
        path_dir = join(path_root, dir_name)
        count_success, count_total = 0, 0
        for filename in tqdm(listdir(path_dir)):
            path = join(path_dir, filename)
            path_prefix, extension = splitext(path)
            if extension.lower() in {'.jpg', '.png'}:
                count_total += 1
                process = subprocess.Popen(shlex.split(bash_command.format(path)), stdout=subprocess.PIPE,
                                           cwd=join(environ['HOME'], 'ProgramFiles', 'opencv_qr'))
                output, error = process.communicate()
                if output:
                    count_success += 1
                    markup = json.loads(output.decode('utf-8'))
                    for corner in markup:
                        corner['pts'] = get_corner_points(corner['pts'])
                    with open(path_prefix + '_markup.json', 'w') as fout:
                        json.dump(markup, fp=fout)
        print(f'{dir_name}: {count_success} / {count_total}')
