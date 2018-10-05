#!/usr/bin/env python2
# -*- coding: utf-8 -*-

"""
Downloads tarballs contianing the source code of the top 500 Hackages.
"""

from os.path import expanduser
from multiprocessing import Pool
from urllib2 import urlopen
from bs4 import BeautifulSoup as soup
from custom_utils.utils import get_soup

def download_tarball(url):

    out_dir = expanduser("~") + "/research/data/inputs/hackage500/"
    soup = get_soup(url)
    hackage_base_url = "http://hackage.haskell.org"
    relative_tarball_link = soup.find(id="downloads").a["href"]
    tarball_download_link = hackage_base_url + relative_tarball_link

    file_name = tarball_download_link.split("/")[-1]
    file_stream = urlopen(tarball_download_link)
    out_path = out_dir + file_name

    with open(out_path, "wb") as tarfile:
        tarfile.write(file_stream.read())


def main():
    pool = Pool(10)
    num_packages_to_download = 500
    hackage_base_url = "http://hackage.haskell.org"
    top_hackages_url = "http://hackage.haskell.org/packages/top"

    top_hackages_soup = get_soup(top_hackages_url)
    all_hackage_links = top_hackages_soup.table.findAll("a")
    all_hackage_urls = map(lambda link: hackage_base_url + link['href'],
                           all_hackage_links)[:num_packages_to_download]
    pool.map(download_tarball, all_hackage_urls)


if __name__ == '__main__':
    main()
