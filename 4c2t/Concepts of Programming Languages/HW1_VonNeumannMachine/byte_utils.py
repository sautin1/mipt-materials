import numpy as np


def byte_array_to_int(byte_array):
    dtype = '>i' + str(len(byte_array))
    return np.fromstring(byte_array.tostring(), dtype=dtype)[0]


def int_to_byte_array(number, size=4):
    byte_str = np.array([number], dtype='>i' + str(size))
    return np.frombuffer(byte_str.tobytes(), dtype=np.ubyte)

if __name__ == '__main__':
    print(int_to_byte_array(1000, size=2))
