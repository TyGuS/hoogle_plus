import os
import tarfile
import subprocess
import progressbar
from multiprocessing import Pool

def create_import_files(tarball_filename):

  devnull = open(os.devnull, 'wb')
  src_dir = os.path.expanduser("~") + "/research/data/working/hackage500/"
  file_path = src_dir + tarball_filename
  p = subprocess.Popen(["stack", "exec", "hplus-bench-utils-exe", file_path], cwd="/home/djusto/research/projects/hplus-bench-utils", shell=False)
  p.wait() 

def main():      

  src_dir = os.path.expanduser("~") + "/research/data/working/hackage500/"
  cnt = 0
  with progressbar.ProgressBar(max_value=(len(tarballs))) as bar:
    for tarball in tarballs:      
      create_import_files(tarball)
      cnt += 1
      bar.update(cnt)

if __name__ == '__main__':
  main()
