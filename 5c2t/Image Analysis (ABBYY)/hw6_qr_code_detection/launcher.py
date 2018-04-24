from os.path import join, basename, dirname, splitext, abspath
from os import listdir, makedirs
import subprocess
from shlex import split


PATH_PROJECT = dirname(abspath(__file__))
PATH_INPUT = join(PATH_PROJECT, 'datasets', 'abbyy')
PATH_OUTPUT = join(PATH_PROJECT, 'results')
BASH_COMMAND = 'build-hw6_qr_code_detection-Desktop_Qt_5_10_1_GCC_64bit-Release/hw6_qr_code_detection "{}" -o "{}" --time'

times = []
for dataset_name in listdir(PATH_INPUT):
    path_input_dataset = join(PATH_INPUT, dataset_name)
    path_output_dataset = join(PATH_OUTPUT, dataset_name)
    for file_name in listdir(path_input_dataset):
        if splitext(file_name)[1].lower() in {'.jpg', '.png'}:
            path_input_file = join(path_input_dataset, file_name)
            path_output_dir = join(path_output_dataset, splitext(file_name)[0])
            makedirs(path_output_dir, exist_ok=True)

            command = BASH_COMMAND.format(path_input_file, path_output_dir)
            process = subprocess.Popen(split(command), stdout=subprocess.PIPE)
            stdout, _ = process.communicate()
            stdout = stdout.decode('utf-8')
            times.append(int(stdout.split()[1][:-2]))
            print(f'{join(dataset_name, file_name)}')
            print(stdout)
            print()

print(f'Mean time: {sum(times) / len(times)}μs')
