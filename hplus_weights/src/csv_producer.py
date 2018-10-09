#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os
import tarfile
import subprocess
import progressbar
from multiprocessing import Pool

def create_import_files(tarball_filename):

  devnull = open(os.devnull, 'wb')
  src_dir = os.path.expanduser("~") + "/research/data/working/hackage500/"
  file_path = src_dir + tarball_filename
  p = subprocess.Popen(["stack", "exec", "extractUsageExamples-exe", file_path], cwd=os.path.expanduser("~")+ "/projects/hplus-bench-utils", shell=False)
  p.wait() 

def main():      

  src_dir = os.path.expanduser("~") + "/research/data/working/hackage500/"
  tarballs = os.listdir(src_dir)[:10]
  cnt = 0
  with progressbar.ProgressBar((len(tarballs))) as bar:
    for tarball in tarballs:      
      create_import_files(tarball)
      cnt += 1
      bar.update(cnt)

if __name__ == '__main__':
  main()
