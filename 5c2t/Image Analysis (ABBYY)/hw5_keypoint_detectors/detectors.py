import numpy as np
import cv2


class Detector:
    def __init__(self, count):
        self._count = count

    def detect_keypoints(self, image):
        raise NotImplementedError

    def get_name(self):
        raise NotImplementedError

    @staticmethod
    def extract_coords_from_keypoints(keypoints):
        return np.array([keypoint.pt for keypoint in keypoints], dtype=np.int)


class DetectorORB(Detector):
    def __init__(self, count, pyramid_factor=1.2):
        super().__init__(count)
        self._pyramid_factor = pyramid_factor
        self._detector = cv2.ORB_create(count, pyramid_factor)

    def detect_keypoints(self, image):
        keypoints = self._detector.detect(image)
        return self.extract_coords_from_keypoints(keypoints)

    def get_name(self):
        return 'orb'


class DetectorShiTomasi(Detector):
    def __init__(self, count, quality_level=0.01, min_distance=10):
        super().__init__(count)
        self._quality_level = quality_level
        self._min_distance = min_distance

    def detect_keypoints(self, image):
        keypoints = cv2.goodFeaturesToTrack(image, self._count, self._quality_level, self._min_distance)
        return keypoints.reshape((-1, 2)).astype(np.int)

    def get_name(self):
        return 'shi-tomasi'


class DetectorSift(Detector):
    def __init__(self, count):
        super().__init__(count)
        self._detector = cv2.xfeatures2d.SIFT_create(count)

    def detect_keypoints(self, image):
        keypoints = self._detector.detect(image)
        return self.extract_coords_from_keypoints(keypoints)

    def get_name(self):
        return 'sift'


if __name__ == '__main__':
    from os.path import join
    from hw5_keypoint_detectors.image import read_image

    path = join('..', 'hw4_motion_vector', 'data', '01.tif')
    image = read_image(path)

    detector = DetectorSift(500)
    res = detector.detect_keypoints(image)
    print(res.shape)
    print(res)
