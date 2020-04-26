from os import system

remove_comments = lambda x: not x.startswith("#")

packages_file = "packages.txt"

def get_package_list():
    with open(packages_file) as f:
        packages = [x.strip() for x in f.readlines()]
        packages = list(filter(remove_comments, packages))
    return packages

def install_package_list(pkgs):
    cmd = f"sudo apt install {' '.join(pkgs)}"
    system(cmd)

if __name__ == "__main__":
    install_package_list(get_package_list())
