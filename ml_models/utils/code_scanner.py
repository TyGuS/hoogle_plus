# HEADER FILE AND UTF

import re
from urllib2 import urlopen
from bs4 import BeautifulSoup as soup


def print_welcome_message():
    """ Prints the starting welcome message for the CLI interface """
    message = "\n\n[ The Hoogle+ Reverse Dependecy Scanner ]\n\n" +\
        "This tool finds hackage packages that depend on a user specified\n" +\
        "library The open source code for these packages is then scanned \n" +\
        "to collect various data and statistics relevant to Hoogle+ \n"
    print message


def get_library_name():
    """
    Prompts the user to input the name of the library whose reverse
    dependencies' source code they wish to analyze.

    Returns:
        str: A hackage library name.
    """
    message = "Please enter the library whose usage you wish to analyze"
    prompt = "Library name: "

    print message
    library_name = raw_input(prompt)
    return library_name


def get_name_and_url(html_element):
    """
    Extracts the text embedded in an `<a/>` tag alongside its url

    Args:
        html_ement (bs4 soup): A bs4 soup element representing an 'a' tag

    Returns:
        (str, str): a tuple of the text inside an a tag alongside its url/href
    """
    name = html_element.text.strip()
    url = html_element["href"]
    return name, url


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


def process_package(url, library_name):

    github_base = "https://github.com"
    package_name = url.split("/")[-1]
    print "\n- Processing package: " + package_name

    text_soup = get_soup(url)
    pattern = re.compile("https://github.com")
    github_candidates = text_soup.table.tbody.findAll(text=pattern)

    if len(github_candidates) == 0:
        print "-- Not GitHub repo was found. Skipping"
        return

    github_url = github_candidates[0].strip()
    if "issues" in github_url:
        github_url = "/".join(github_url.split("/")[:-1])
    print "-- Found its GitHub repo at: " + github_url

    search_parameter = "/search?q=" + library_name
    search_url = github_url + search_parameter
    text_soup = get_soup(search_url)
    results = text_soup.findAll("div", {"class": "code-list-item"})
    for usage_of_library_div in results:
        file_relative_url = usage_of_library_div.a["href"]
        file_url = github_base + file_relative_url
        file_soup = get_soup(file_url)
        raw_file_relative_url = file_soup.findAll("a",
            {"id": "raw-url"})[0]["href"]
        raw_file_url = github_base + raw_file_relative_url
        raw_file_name = raw_file_url.split("/")[-1]

        if ".hs" not in raw_file_url:
            print "-- skipping file: " + raw_file_name + ". Not a Haskell file"
        else:
            print "-- processing file: " + raw_file_name

        raw_file_data = get_soup(raw_file_url)
        #print raw_file_data
    # handle the case of multiple pages





def main():
    reverse_dependency_url = "https://packdeps.haskellers.com/reverse"
    reverse_dependency_soup = get_soup(reverse_dependency_url)

    in_use = True
    print_welcome_message()
    library_name = get_library_name()
    library_name = library_name.strip()

    if library_name == "":
        print "You did not enter a valid name. Goodbye"
        exit()

    library_filter = lambda x: library_name == x[0].strip()
    libraries = reverse_dependency_soup.findAll("a", {})
    names_and_urls = map(get_name_and_url, libraries)
    library_matches = filter(library_filter, names_and_urls)

    if len(library_matches) == 0:
        print "Library not found. Goodbye"
        exit()

    [(_, library_url)] = library_matches
    packages_to_scan_soup = get_soup(library_url)
    packages = packages_to_scan_soup.table.tbody.findAll("td",
        {"class":"version"})
    packages_urls = map(lambda x: x.a["href"], packages)

    for candidate_package_url in packages_urls:
        process_package(candidate_package_url, library_name)


if __name__ == '__main__':
    main()
