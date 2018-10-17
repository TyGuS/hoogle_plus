from os import listdir, devnull
from subprocess import Popen
from custom_utils.utils import (get_data_path)
from generate_identifiers_maps import (generate_identifier_maps)


def create_map_files(src_dir, tarball_filename):
  file_path = src_dir + tarball_filename
  generate_identifier_maps(file_path)


def main():
  src_dir = get_data_path("working/hackage500Imports/")
  tarballs = listdir(src_dir)
  total = len(tarballs)
  cnt = 0
  for tarball in tarballs:      
    create_map_files(src_dir, tarball)
    cnt += 1
    if cnt % 20 == 0:
        print("%d/%d" %(cnt, total))


if __name__ == '__main__':
  main()
