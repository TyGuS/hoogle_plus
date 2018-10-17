#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os
import subprocess
from custom_utils.utils import (get_data_path, get_repo_root)


def create_import_files(tarball_filename):
  devnull = open(os.devnull, 'wb')
  src_dir = get_data_path("working/hackage500/")
  file_path = src_dir + tarball_filename
  p = subprocess.Popen(["stack", "exec", "extractUsageExamples-exe", file_path],
                       cwd=get_repo_root() + "/hplus-bench-utils", shell=False)
  p.wait() 


def main():
    src_dir = get_data_path("working/hackage500/")
    tarballs = os.listdir(src_dir)[:10]
    cnt = 0
    for tarball in tarballs:
        create_import_files(tarball)
        cnt += 1


if __name__ == '__main__':
  main()
