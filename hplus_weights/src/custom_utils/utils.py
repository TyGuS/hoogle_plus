#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from urllib2 import urlopen
from bs4 import BeautifulSoup as soup



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
