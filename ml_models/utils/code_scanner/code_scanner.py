#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
code_scanner.py is a script that leverages BeautifulSoup to scrape haskell
source code from GitHub that depends on a user-specified library.

It UI is on the command line. Assuming you have all dependencies installed,
use it as 'python code_scanner.py' on a terminal
"""

import re
import os
import sys
import time
import json
import pickle
import logging
import traceback
import exceptions

from urllib2 import urlopen
from time import gmtime, strftime
from bs4 import BeautifulSoup as soup  # Library for web scraping
from collections import Counter, defaultdict

# TODO: signature of 'library_api' is wrong in string docs. fix it


def setup_logging_to_file(filename):
    logging.basicConfig(filename=filename,
                        filemode='w',
                        level=logging.DEBUG,
                        format='%(asctime)s - %(levelname)s - %(message)s'
                        )


def extract_function_name():
    """Extracts failing function name from Traceback
    by Alex Martelli
    http://stackoverflow.com/questions/2380073/\
    how-to-identify-what-function-call-raise-an-exception-in-python
    """
    tb = sys.exc_info()[-1]
    stk = traceback.extract_tb(tb, 1)
    fname = stk[0][3]
    return fname


def log_exception(e):
    logging_message = "Function {function_name} "
    logging_message += "raised {exception_class} ({exception_docstring}): "
    logging_message += "{exception_message}"
    logging.error(logging_message.format(
        function_name=extract_function_name(),
        exception_class=e.__class__,
        exception_docstring=e.__doc__,
        exception_message=e.message))


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
    url = html_element["href"].strip()
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


def extract_github_link(url):
    """
    Given a hackage url, find the link to its GitHub repo, if it exists
    Args:
        url (str): a url to the hackage website of hackage
    Returns:
        github_url (str): a github url. Empty if no GitHub url was found
    """

    github_url = ""

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
        return github_url

    # EDGE CASE: In case we grabbed the `issues` github link instead of the
    # link to the repository's root directory, then we fix the url by removing
    # `/issues` from it.
    github_url = github_candidates[0].strip()
    if "issues" in github_url:
        github_url = "/".join(github_url.split("/")[:-1])
    print "-- Found its GitHub repo at: " + github_url

    return github_url


def process_package(url, library_name, public_api, function_counts):
    """
    Takes a hackage url (a reverse dependency of the library specified by
    the user), traverses the html until it finds its source code and analyzes
    that source code. To analyze source code means to extract statistcs on the
    usage of the user-specified library in the source code of the reverse
    dependency

    Args:
        url (string): The hackage url of the reverse dependency
        library_name (string): the name of the user specified library
        public_api (dict): dictionary of the format {<str>:<str>} mapping
          function names to their haskell type-signatures
        function_counts (dict): dictionary of the format {<str> : <int> }
          which serves to count how many times a function has been used
          across its reverse dependencies

    Returns:
        function_counts (dict): dictionary of the format {<str> : <int> }
          which serves to count how many times a function has been used
          across its reverse dependencies

    """
    # TODO: handle the case of multiple pages

    # Attempt to extract GitHub url, return if not found
    github_base = "https://github.com"
    github_url = extract_github_link(url)
    if github_url == "":
        return function_counts

    # Now we search for instances of usage of the library in the repo
    # TODO: hackages sometimes export more than one root-level module,
    #       handle those cases
    search_parameter = "/search?q=" + library_name
    search_url = github_url + search_parameter
    try:
        text_soup = get_soup(search_url)
    except exceptions.Exception as e:
        print "HTTP Error. Skipping"
        log_exception(e)
        return function_counts

    # For each file that returns a usage of the hackage ....
    files_using_library = text_soup.findAll("div", {"class": "code-list-item"})
    for usage_of_library_div in files_using_library:

        # We request the `raw` version of the source file
        file_relative_url = usage_of_library_div.a["href"]
        file_url = github_base + file_relative_url
        try:
            file_soup = get_soup(file_url)
        except exceptions.Exception as e: #404 errors may occurs
            log_exception(e)
            continue
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
            # Increase function usage counts in function_counts
            print "-- processing file: " + raw_file_name
            raw_file_data_soup = get_soup(raw_file_url)
            function_counts = record_usage(raw_file_data_soup, public_api,
                                           function_counts)
    return function_counts


def record_usage(raw_file_data_soup, public_api, function_counts):
    """
    It uses the function_counts parameter to record how many times a
    function was used in the raw source code encapsulated in raw_file_data_soup

    Args:
        raw_file_data_soup (bs4 soup): a bs4 soup that encapsulates a single
          source code file of a reverse dependency
        public_api (dict): dictionary of the format {<str>:<str>} mapping
          function names to their haskell type-signatures
        function_counts (dict): dictionary of the format {<str> : <int> }
          which serves to count how many times a function has been used
          across its reverse dependencies
    Returns:
        function_counts (dict): dictionary of the format {<str> : <int> }
          which serves to count how many times a function has been used
          across its reverse dependencies
    """

    mapper = {}  # translates a function as referenced according to its
    # namespace to the its corresponding key value in
    # function_counts
    modules_imported = []   # module imported in the source code
    functions_to_check = []  # functions corresponding to the modules imported

    # Convert the bs4 soup into raw text, then get each line
    raw_file_data = soup.prettify(raw_file_data_soup)
    all_lines = raw_file_data.split("\n")

    # Find all import statements
    pattern = re.compile("import")
    module_names = public_api.keys()
    import_statements = filter(lambda x: pattern.search(x), all_lines)

    # find which modules from the library are being imported
    for import_statement in import_statements:
        for module in module_names:

            # If the module is imported, record it
            if module in import_statement:

                modules_imported.append(module)

                # EDGE CASE: If the module being imported has not functions
                # in it. Then perhaps we're dealing with a version mismatch.
                # Ignore thus cases.
                try:
                    functions, _ = zip(*public_api[module])
                except exceptions.Exception as e:
                    log_exception(e)
                    print "--- Issue with module: " + module
                    print "--- This happens when you have a version mismatch"
                    print "--- Ignoring the usage of this module"

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

    # We look for all usages of each function and increase their counts
    for function in functions_to_check:

        results = re.findall(function, raw_file_data)
        if len(results) > 0:
            #print function
            #print mapper
            #print "Data.ByteString.null" in function_counts.keys()
            #print "-------"
            #print function_counts.keys()
            try:
                function_counts[mapper[function]] += len(results)
            except exceptions.Exception as e1:
                log_exception(e1)
                try:
                    function_counts[function] += len(results)
                except exceptions.Exception as e2:
                    log_exception(e2)
                    exit()



    return function_counts


def get_library_tuple(library_name):
    """
    Given a library name, it first checks if that library has a local
    file indicating all ifs public-facing methods and their respective
    type signatures. If not, it generates that file.

    That file gets loaded and parsed. It then gets converted into the
    `library_api` dictionary of format `{<str>: <str>}` where the keys
    are method names and the values are their haskell type-signatures.

    It also constructs function counts, a dictionary that will later allow
    us to count how many times a function is used in source code that
    depends on it.

    Args:
        library_name (str): the name of the library whose metadata we wish
            to generate.
    Returns:
        library_api (dict): {<str>: <str>}.
        function_counts (dict): dictionary of the format {<str> : <int> }
          which serves to count how many times a function has been used
          across its reverse dependencies
    """
    # TODO: handle the case of generating the text file

    function_counts = defaultdict(int)

    # Load local file with public-facing methods of library_name
    # and process it line by line
    path = "./library_function_lists/" + library_name + "_function_list.txt"
    library_api = {}
    with open(path) as file_stream:

        file_stream.next()  # skip first line (just the library name)
        current_module_header = ""

        for line in file_stream:

            # In case we encounter a new module (followed by its functions)
            if ("::" not in line) and (" " not in line):
                current_module_header = line.strip()
                library_api[current_module_header] = set()

            # Not a function. Not important. This case skips data declarations
            # and similar structures
            elif "::" not in line:

                continue
            # We have found a function
            else:
                [method_path, method_signature] = line.split("::")
                method_tuple = (method_path.strip(), method_signature.strip())
                library_api[current_module_header].add(method_tuple)
                function_counts[method_path.strip()]

    return library_api, function_counts


def main():
    """ Program's entry point. It defines the user interaction loop """

    # The delay (in seconds) inbetween requesting GitHub source code.
    # It prevents HTTP 429 (too many requests) errors
    delay_in_seconds = 5

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

    # public_api is a dictionary {<str>:<str>} of function names to their
    # type signatures. function_counts is a dictionary {<str:<int>} of function
    # names to numbers that will count the number of instances a function was
    # called in open source code.
    public_api, function_counts = get_library_tuple(library_name)

    # we check if a checkpoints file exists. If so, we let the user know
    # and ask if they want to use it or start anew.
    libraries_analyzed = set()
    num_libraries_analyzed = 0
    checkpoint_libraries_to_skip = "./checkpoints/"
    checkpoint_libraries_to_skip += "%s_already_processed.pkl"
    checkpoint_libraries_to_skip = checkpoint_libraries_to_skip % \
        library_name

    if os.path.isfile(checkpoint_libraries_to_skip):
        message = "\nThis library has been processed before.\n"
        message += "Do you want to continue from where you left off?"
        print message
        response = raw_input("> [y/n]: ")

        if response == "y":
            print "Ok! Loading checkpoint files"
            with open(checkpoint_libraries_to_skip, 'r') as f:
                libraries_analyzed = pickle.load(f)
                num_libraries_analyzed = len(libraries_analyzed)

            checkpoint_filename = "./checkpoints/"
            checkpoint_filename += "%s_intermediate_counts_after_%d.json" % \
                (library_name, num_libraries_analyzed)

            with open(checkpoint_filename, 'r') as f:
                function_counts = json.load(f)

        else:
            print "You selected either 'No' or an invalid option."
            print "Will not use the checkpoint."
            num_libraries_analyzed = 0
            libraries_analyzed = set()

    # Scan the html table in the 'reverse dependency' site for a link
    # to the user-specified library
    libraries = reverse_dependency_soup.findAll("a", {})
    names_and_urls = map(get_name_and_url, libraries)
    library_matches = filter(lambda x: library_name == x[0], names_and_urls)

    # If the user-specified library was not found in the site, then terminate
    if len(library_matches) == 0:
        print "Library not found. Goodbye"
        exit()

    # Using the 'Reverse Depency Monitor', we get a url to the list of
    # hackages that depend on the user-specified library.
    [(_, library_url)] = library_matches
    packages_to_scan_soup = get_soup(library_url)

    # We collect at list of links leading to hackage websites of reverse
    # dependencies.
    packages = packages_to_scan_soup.\
        table.tbody.findAll("td", {"class": "version"})
    packages_urls = map(lambda x: x.a["href"], packages)

    # We attempt to analyze each reverse dependency

    for candidate_package_url in packages_urls:
        if candidate_package_url in libraries_analyzed:
            print "- Due to checkpoint, we're skipping library at url..."
            print "-- " + candidate_package_url
            continue

        function_counts = process_package(candidate_package_url, library_name,
                                          public_api, function_counts)
        num_libraries_analyzed += 1
        libraries_analyzed.add(candidate_package_url)

        # Sleep after each analysis to prevent 429 HTTP errors
        time.sleep(delay_in_seconds)

        # Report current library function counts
        usage_counts = Counter(function_counts)
        counting_message = "-- Top 10 most used functions thus far: %r"
        print counting_message % usage_counts.most_common(10)

        # Saving every 30 libraries are analyzed
        if num_libraries_analyzed % 50 == 0:
            checkpoint_filename = "./checkpoints/"
            checkpoint_filename += "%s_intermediate_counts_after_%d.json" % \
                (library_name, num_libraries_analyzed)
            print "- Checkpoint: writing intermediate output "
            print "-- We have processed a total of %d libraries" % \
                num_libraries_analyzed
            with open(checkpoint_filename, 'w') as f:
                print function_counts.keys()[:4]
                json.dump(function_counts, f)

            # Also store which reverse dependencies have already been processed
            # such that they can be skipped at a later run
            with open(checkpoint_libraries_to_skip, 'w') as f:
                pickle.dump(libraries_analyzed, f)

    # Saving the final counts dict
    final_counts_filename = "./outputs/%s_final_counts.json" % library_name
    with open(final_counts_filename, 'w') as f:
        json.dump(function_counts, f)


if __name__ == '__main__':
    timestamp = strftime("%Y-%m-%d %H:%M:%S", gmtime())
    log_filename = "./logs/code_scanner_%r.log" % timestamp
    setup_logging_to_file(log_filename)
    main()
