def calc_bounding_box(points):
    return (
        (min(point[0] for point in points), min(point[1] for point in points)),
        (max(point[0] for point in points), max(point[1] for point in points))
    )


def clip_bounding_box(box, image_shape):
    return ((max(box[0][0], 0), max(box[0][1], 0)),
            (min(box[1][0], image_shape[1]), min(box[1][1], image_shape[2])))
