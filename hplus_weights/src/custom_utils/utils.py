#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os
import git
from urllib2 import urlopen
from bs4 import BeautifulSoup as soup


DATA_DIR = os.path.join('hplus_weights', 'output', 'data')


def get_soup(url):
    """
    Given a url string, it returns a bs4 soup element for easy html parsing

    Args:
        url (string): a url string

    Returns:
        (bs4 soup): A bs4 soup element of the html corresponding to the url
    """

    url_stream = urlopen(url)
    page_html = url_stream.read()
    url_stream.close()
    text_soup = soup(page_html, "html.parser")
    return text_soup


def get_repo_root():
    git_repo = git.Repo(os.path.abspath(__file__), search_parent_directories=True)
    git_root = git_repo.git.rev_parse("--show-toplevel")
    return os.path.abspath(git_root)


def get_data_path(data_folder_name):
    """
    Given a folder path, create the parent structure and path necessary to access that folder.

    Args:
        path_rel_repo_root (str)

    Return:
        absolute path to folder
    """
    data_dir_path = os.path.join(get_repo_root(), DATA_DIR, data_folder_name)
    if not os.path.exists(data_dir_path):
        os.makedirs(data_dir_path)
    return data_dir_path

