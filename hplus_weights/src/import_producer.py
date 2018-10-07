import os
import tarfile
import subprocess
from multiprocessing import Pool

def create_import_files(tarball_filename):

  devnull = open(os.devnull, 'wb') #python >= 2.4
  src_dir = os.path.expanduser("~") + "/research/data/working/hackage500/"
  file_path = src_dir + tarball_filename
  p = subprocess.Popen(["stack", "exec", "extractImports", file_path], cwd="/home/djusto/research/projects/hplus-bench-utils", shell=False)
  p.wait() 

def main():      

  src_dir = os.path.expanduser("~") + "/research/data/working/hackage500/"
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

