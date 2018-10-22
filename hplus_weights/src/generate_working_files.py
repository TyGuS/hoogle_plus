#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from os import listdir
from multiprocessing import Pool
from tarfile import open as opentar
from custom_utils.utils import get_data_path


def process_tarball(tarball_filename):

  src_dir = get_data_path("inputs/hackage500/")
  out_dir = get_data_path("working/hackage500/")

  tarball_path = src_dir + tarball_filename
  tarball_stream = opentar(tarball_path, 'r')

  pragmas =  "{-# LANGUAGE OverloadedStrings #-}\n{-# LANGUAGE ScopedTypeVariables #-}\n{-# LANGUAGE LambdaCase #-}\n{-# LANGUAGE TemplateHaskell #-}\n{-# LANGUAGE TypeFamilies #-}\n{-# LANGUAGE FlexibleContexts #-}\n{-# LANGUAGE ExplicitNamespaces #-}\n{-# LANGUAGE BangPatterns #-}\n{-# LANGUAGE MultiParamTypeClasses #-}\n{-# LANGUAGE TupleSections #-}\n{-# LANGUAGE TupleSections #-}\n{-# LANGUAGE RecordWildCards #-}\n{-# LANGUAGE PackageImports #-}\n{-# LANGUAGE NamedFieldPuns #-}\n{-# LANGUAGE GADTs #-}\n{-# LANGUAGE CPP #-}\n{-# LANGUAGE DataKinds #-}\n{-# LANGUAGE StandaloneDeriving #-}\n{-# LANGUAGE FunctionalDependencies #-}\n{-# LANGUAGE ViewPatterns #-}\n{-# LANGUAGE InstanceSigs #-}\n{-# LANGUAGE DefaultSignatures #-}\n{-# LANGUAGE ExistentialQuantification #-}\n"

  for tarball_member in tarball_stream.getnames():
    if ".hs" in tarball_member:
      file_as_string = str(tarball_stream.extractfile(tarball_member).read())
      file_as_string = pragmas + file_as_string

      if "ByteString" in file_as_string:
        out_path = out_dir + tarball_member.replace("/", "_")
        with open(out_path, "w") as f:
          f.write(file_as_string)


def main():
  pool = Pool(10)
  src_dir = get_data_path("inputs/hackage500/")
  tarballs = list(filter(lambda x: "tar.gz" in x, listdir(src_dir)))
  pool.map(process_tarball, tarballs)

if __name__ == '__main__':
  main()
