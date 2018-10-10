import os
import subprocess
from custom_utils.utils import (get_data_path, get_repo_root)

def create_import_files(tarball_filename):

  devnull = open(os.devnull, 'wb') #python >= 2.4
  src_dir = get_data_path("working/hackage500/")
  file_path = src_dir + tarball_filename
  root = get_repo_root()
  bench_path = os.path.join(root, "hplus-bench-utils")
  p = subprocess.Popen(["stack", "exec", "extractImports-exe", file_path], cwd=bench_path, shell=False)
  p.wait() 

def main():      

  src_dir = get_data_path("working/hackage500/")
  tarballs = os.listdir(src_dir)
  total = len(tarballs)
  cnt = 0
  for tarball in tarballs:      
    create_import_files(tarball)
    cnt += 1
    if cnt % 50 == 0:
      print "%d/%d" % (cnt, total)

if __name__ == '__main__':
  main()

