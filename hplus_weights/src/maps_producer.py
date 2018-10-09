from os.path import expanduser
from os import listdir, devnull
from subprocess import Popen

def create_map_files(tarball_filename):

  src_dir = expanduser("~") + "/research/data/working/hackage500Imports/"
  file_path = src_dir + tarball_filename
  p = Popen(["python", "generate_identifiers_maps.py", file_path], shell=False)
  p.wait() 

def main():      

  src_dir = expanduser("~") + "/research/data/working/hackage500Imports/"
  tarballs = listdir(src_dir)
  total = len(tarballs)
  cnt = 0
  for tarball in tarballs:      
    create_map_files(tarball)
    cnt += 1
    if cnt % 20 == 0:
        print "%d/%d" %(cnt, total)

if __name__ == '__main__':
  main()
