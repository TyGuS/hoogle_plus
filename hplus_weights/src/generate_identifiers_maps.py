import re
import os
import sys
import json
import string

from enum import Enum
from collections import defaultdict

class ImportMode(Enum):
    invalid = 0
    qualified = 1
    hiding = 2
    explicit = 3
    full = 4

def load_supported_APIs():
    home_path = os.path.expanduser('~') 
    supported_modules_dir = home_path + \
        "/research/data/inputs/hplusSupported/"
    supported_module_path = supported_modules_dir + "bytestring.api"

    module_functions_map = defaultdict(set)
    supported_modules = set()
    with open(supported_module_path, 'r') as file_stream:
        for line in file_stream:
            if "::" in line:
                qualified_function_name = line.split("::")[0]
                function_name_components = qualified_function_name.split(".")
                function_name = function_name_components[-1].strip()
                function_qual = '.'.join(function_name_components[:-1])
                (module_functions_map[function_qual]).add(function_name)
                supported_modules.add(function_qual)

    return supported_modules, module_functions_map


def _extract_import_contents(import_statement, supported_modules):

    import_statementBAK = import_statement
    if len(import_statement) == 0:
        return (ImportMode.invalid, (None,))

    # remove useless keywords
    import_statement = import_statement.replace("import", '')
    import_statement = import_statement.replace("qualified", '')


    # Extract the module being imported
    import_components = import_statement.split()
    if len(import_components) == 0:
        print "PROB"
        return (ImportMode.invalid, (None,))
    module_name = import_components[0]
    remaining_components = import_components[1:] 

    # Case 0: the module imported is not supported
    if module_name not in supported_modules:
        return (ImportMode.invalid, (None,))

    # Case 1: Is a qualified statement
    # TODO: create custom logging exception
    if 'as' in remaining_components:
        if len(remaining_components) < 2:
            raise Exception
        alias = remaining_components[1]
        return_val = (ImportMode.qualified, (module_name, alias))
        
        
    # Case 2: imports all public API except selected functions
    elif 'hiding' in remaining_components:
        hidden_functions = ''.join(remaining_components)
        hidden_functions = hidden_functions.replace('hiding', '').replace('(','').replace(')','')
        hidden_functions = hidden_functions.split(',')
        #print hidden_functions, "$$$$$$$$", import_statement
        return_val = (ImportMode.hiding, (module_name, hidden_functions))

    # Case 3: imports only selected functions
    elif '(' in ''.join(remaining_components):
        exported_functions = ''.join(remaining_components)
        exported_functions = exported_functions.replace('(','').replace(')','')
        exported_functions = exported_functions.split(',')
        return_val = (ImportMode.explicit, (module_name, exported_functions))

    # Case 4: imports all functions
    else:
        return_val = (ImportMode.full, module_name)

    return return_val 


def get_qualifying_maps(import_statements):

    qualification_map = {}
    explicit_names_map = {}

    #print import_statements

    supported_modules, module_maps = load_supported_APIs()
    import_contents = map(lambda x: \
        _extract_import_contents(x, supported_modules), import_statements)


    for (import_mode, import_content) in import_contents:
        #print import_mode
        if import_mode == ImportMode.qualified:
            module_name, alias = import_content
            qualification_map[alias] = module_name

        elif import_mode == ImportMode.explicit:

            module_name, exported_functions = import_content
            for function_name in exported_functions:
                explicit_names_map[function_name] = module_name + "." + function_name
                
        elif import_mode == ImportMode.hiding:
            module_name, hidden_functions = import_content
            module_function_set = module_maps[module_name].copy()
            for function_name in hidden_functions:
                #print module_name, hidden_functions
                try:
                    module_function_set.remove(function_name)
                except:
                    continue

            for function_name in module_function_set:
                explicit_names_map[function_name] = module_name + "." + function_name

        elif import_mode == ImportMode.full:
            module_name = import_content
            module_function_set = module_maps[module_name]
            for function_name in module_function_set:
                explicit_names_map[function_name] = module_name + "." + function_name
 
    return qualification_map, explicit_names_map 


    

def main():
    
    # File path to the haskell file to process
    file_path = sys.argv[1]
    
    
    hs_imports = ""
    with open(file_path, 'r') as f:
        hs_imports = f.read()
    
    hs_imports = hs_imports.split('\n')
    qualification_map, explicit_names_map = get_qualifying_maps(hs_imports)
    file_name = file_path.split("/")[-1].replace('.txt', '')
    output_file_dir = os.path.expanduser("~") + "/research/data/working/hackage500Maps/"

    qual_map_outpath = output_file_dir + file_name + "_qual.json" 
    expl_map_outpath = output_file_dir + file_name + "_expl.json"
    with open(qual_map_outpath, 'w') as f:
        json.dump(qualification_map,f )

    with open(expl_map_outpath, 'w') as f:
        json.dump(explicit_names_map, f)
    



# Run the main function when this file is runned as a script
if __name__ == '__main__':
    main()
