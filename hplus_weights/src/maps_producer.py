from os.path import expanduser("~")
from os import listdir, devnull
from subprocess import Popen
from progressbar import ProgressBar

def create_map_files(tarball_filename):

  devnull = open(devnull, 'wb') #python >= 2.4
  src_dir = expanduser("~") + "/research/data/working/hackage500Imports/"
  file_path = src_dir + tarball_filename
  p = Popen(["python", "hs_transformer.py", file_path], shell=False)
  p.wait() 

def main():      

  src_dir = expanduser("~") + "/research/data/working/hackage500Imports/"
  tarballs = listdir(src_dir)
  cnt = 0
  with ProgressBar(max_value=(len(tarballs))) as bar:
    for tarball in tarballs:      
      create_map_files(tarball)
      cnt += 1
      bar.update(cnt)

if __name__ == '__main__':
  main()
