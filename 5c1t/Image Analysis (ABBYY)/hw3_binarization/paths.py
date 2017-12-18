import os


PATH_PROJECT = os.path.dirname(os.path.abspath(__file__))
PATH_DATA = os.path.join(PATH_PROJECT, 'data')
PATH_RESULTS = os.path.join(PATH_PROJECT, 'results')


def list_files(root, extensions=None):
    file_names = os.listdir(root)
    if extensions:
        file_names = filter(lambda file_name: os.path.splitext(file_name) in extensions, file_names)
    return (os.path.join(root, name) for name in file_names)
