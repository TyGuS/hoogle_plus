#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
code_scanner.py is a script that leverages BeautifulSoup to scrape haskell
source code from GitHub that depends on a user-specified library.

It UI is on the command line. Assuming you have all dependencies installed,
use it as 'python code_scanner.py' on a terminal
"""

import re
import time
import json
from urllib2 import urlopen
from collections import Counter, defaultdict
from bs4 import BeautifulSoup as soup  # Library for web scraping


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


def process_package(url, library_name, metadata, all_functions):
    """
    Takes a hackage url (a reverse dependency of the library specified by
    the user), traverses the html until it finds its source code and processes
    that source code
        Args:
            url (string): The hackage url of the reverse dependency
            library_name (string): the name of the user specified library
            metdata (dict): dictionary of the format
                            {"module_name": set((method_name, type_signature))}
            all_functions (dict): dictionary of the format
                                  {"function_name": <int> }
    """

    # We report the library name, which can be extracted from the hackage url
    github_base = "https://github.com"
    package_name = url.split("/")[-1]
    print "\n- Processing package: " + package_name

    # We attempt to find a github link from the html returned by the url
    text_soup = get_soup(url)
    pattern = re.compile("https://github.com")
    github_candidates = text_soup.table.tbody.findAll(text=pattern)

    # If no github repo was found, just return
    if len(github_candidates) == 0:
        print "-- Not GitHub repo was found. Skipping"
        return all_functions

    # EDGE CASE: In case we grabbed the `issues` github link instead of the
    # link to the repository's root directory, then we fix the url by removing
    # `/issues` from it.
    github_url = github_candidates[0].strip()
    if "issues" in github_url:
        github_url = "/".join(github_url.split("/")[:-1])
    print "-- Found its GitHub repo at: " + github_url

    # Now we search for instances of usage of the library in the repo
    # TODO: hackages sometimes export more than one root-level module,
    #       handle those cases
    search_parameter = "/search?q=" + library_name
    search_url = github_url + search_parameter
    try:
        text_soup = get_soup(search_url)
    except urllib2.HTTPError as err:
        print "HTTP Error. Skipping"
        return all_functions

    results = text_soup.findAll("div", {"class": "code-list-item"})

    # For each file that returns a usage of the hackage ....
    for usage_of_library_div in results:

        # We request the `raw` version of the source file
        file_relative_url = usage_of_library_div.a["href"]
        file_url = github_base + file_relative_url
        file_soup = get_soup(file_url)
        raw_file_relative_url = file_soup.\
            findAll("a", {"id": "raw-url"})[0]["href"]
        raw_file_url = github_base + raw_file_relative_url
        raw_file_name = raw_file_url.split("/")[-1]

        # If it turns out that the file where the library is mentioned is not
        # a Haskell (.hs) file (it may be a meta-data file such as .cabal),
        # then we ignore it but we report this 'edge case' to the user as this
        # may be un-intended behaviour
        if ".hs" not in raw_file_url:
            print "-- skipping file: " + raw_file_name + ". Not a Haskell file"
        else:
            print "-- processing file: " + raw_file_name
            raw_file_data_soup = get_soup(raw_file_url)
            all_functions = counter(raw_file_data_soup, metadata,
                                    all_functions)
    # TODO: handle the case of multiple pages
    return all_functions


def counter(raw_file_data_soup, metadata, all_functions):
    # TODO: Refactor w/ DRY. Too much repetition here.
    # TODO: Give function header
    # TODO: rename this function. Poor name
    mapper = {}
    modules_imported = []
    functions_to_check = []

    # Convert the bs4 soup into raw text, the get each line
    raw_file_data = soup.prettify(raw_file_data_soup)
    all_lines = raw_file_data.split("\n")

    # Find all import statements
    pattern = re.compile("import")
    module_names = metadata.keys()
    import_statements = filter(lambda x: pattern.search(x), all_lines)

    # Among the import statements, find which modules from the library of
    # interest were used.
    for import_statement in import_statements:
        for module in module_names:

            # If the module is imported
            if module in import_statement:
                modules_imported.append(module)
                functions, _ = zip(*metadata[module])

                # boolean conditions that change the import behaviour
                is_qualified = "qualified" in import_statement
                is_renamed = "as" in import_statement

                # The following sequence of if-elif-else statements records
                # which patterns (function calls) to look for in the source
                # code given how the module was imported. In addition,
                # it populates the `mapper` dictionary with information
                # to properly `map` patterns to their canonical names. This
                # will enable us to count how many time each function is
                # called.

                # If qualified and renamed ...
                if is_qualified and is_renamed:

                    # we rename the module signature to the new name
                    qual = import_statement.split("as")[-1].strip()
                    qual_funcs = map(lambda x: x.replace(module, qual),
                                     functions)
                    functions_to_check += qual_funcs

                    # we tell the mapper how to go from the new function
                    # names to their canonical names
                    translations = zip(qual_funcs, functions)
                    for new_name, canonical_name in translations:
                        mapper[new_name] = canonical_name

                # If it is simply qualified (and not renamed)...
                elif is_qualified:
                    # the pattern to search for is the canonical name
                    functions_to_check += functions

                    # for ease of computation, we give a trivial translation
                    translations = zip(functions, functions)
                    for new_name, canonical_name in translations:
                        mapper[new_name] = canonical_name

                # If no namespace is provided ...
                else:
                    # remove the module prefix as the functions are imported
                    # to the global namespace
                    qual_funcs = map(lambda x: x.split(".")[-1], functions)
                    functions_to_check += functions

                    translations = zip(qual_funcs, functions)
                    for new_name, canonical_name in translations:
                        mapper[new_name] = canonical_name

    for function in functions_to_check:

        results = re.findall(function, raw_file_data)
        if len(results) > 0:
            all_functions[mapper[function]] += len(results)

    return all_functions


def main():
    """ Program's entry point. It defines the user interaction loop """

    delay_in_seconds = 10

    # We use the haskell 'reverse dependency' site to fetch hackages
    # that depend on a specific library
    reverse_dependency_url = "https://packdeps.haskellers.com/reverse"
    reverse_dependency_soup = get_soup(reverse_dependency_url)

    # Welcome the user and request for a library name
    print_welcome_message()
    library_name = get_library_name()
    library_name = library_name.strip()

    # Edge case: the user does not enter a library name. Instead, they enter
    # spaces or nothing. If so, terminate.
    if library_name == "":
        print "You did not enter a valid name. Goodbye"
        exit()

    # Scan the html table in the 'reverse dependency' site for a link
    # to the user-specified library
    libraries = reverse_dependency_soup.findAll("a", {})
    names_and_urls = map(get_name_and_url, libraries)
    library_matches = filter(lambda x: library_name == x[0].strip(),
                             names_and_urls)

    # If the user-specified library was not found in the site, then terminate
    if len(library_matches) == 0:
        print "Library not found. Goodbye"
        exit()

    # Get the library metadata
    metadata, all_functions = get_library_metadata(library_name)

    # Assuming the user-specified library was found, we get the url
    # that redirects to a list of hackages that depend on that library.
    # We then use beautiful soup to get the urls of the hackage homesites
    # for those hackages. The expectation is that those homesites
    # will lead to open source code (on github) that we can then analize.
    # Thus, we collect all those hackage homesite urls for further processing
    [(_, library_url)] = library_matches
    packages_to_scan_soup = get_soup(library_url)
    packages = packages_to_scan_soup.\
        table.tbody.findAll("td", {"class": "version"})
    packages_urls = map(lambda x: x.a["href"], packages)

    # We process all hackages that are reverse dependencies
    counter = 0
    for candidate_package_url in packages_urls:
        all_functions = process_package(candidate_package_url, library_name,
                                        metadata, all_functions)
        time.sleep(delay_in_seconds)
        cc = Counter(all_functions)
        print "MOST COMMON: %r" % cc.most_common(10)

    with open('my_dict.json', 'w') as f:
        json.dump(my_dict, f)


def get_library_metadata(library_name):
    """
    Given a library name, it first checks if that library has a local
    file indicating all ifs public-facing methods and their respective
    type signatures. If not, it generates that file.

    That file gets loaded and parsed. It then gets converted into the
    `library_metadata` dictionary of format `{function_name: type_signature}`.
    Lastly, we create a dictionary `all_functions` of format
    `{function_name: 0}`. The idea is that `all_functions` will later be used
    to count how many times a function was called in the open source code
    that is being scraped.
    Args:
        library_name (str): the name of the library whose metadata we wish
            to generate.
    Returns:
        library_metadata (dict):
            dictionary of format `{function_name: type_signature}`.
        all_functions (dict): dictionary of format
            `{function_name: 0}
    """
    # TODO: handle the case of generating the text file
    path = library_name + ".txt"
    library_metadata = {}
    all_functions = defaultdict(int)
    with open(path) as file_stream:
        file_stream.next()  # skip first line (just the library name)
        current_module_header = ""
        for line in file_stream:

            if ("::" not in line) and (" " not in line):
                current_module_header = line.strip()
                library_metadata[current_module_header] = set()
            else:
                if "::" not in line:
                    # Not a function. Not important
                    # This case skips data declarations
                    # and similar structures
                    continue
                [method_path, method_signature] = line.split("::")
                method_tuple = (method_path.strip(), method_signature.strip())
                library_metadata[current_module_header].add(method_tuple)
                all_functions[method_path]
    return library_metadata, all_functions


if __name__ == '__main__':
    main()
