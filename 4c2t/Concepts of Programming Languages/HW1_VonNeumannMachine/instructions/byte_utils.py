import numpy as np


def byte_array_to_int(byte_array):
    dtype = '>i' + str(len(byte_array))
    return np.fromstring(byte_array.tostring(), dtype=dtype)[0]
