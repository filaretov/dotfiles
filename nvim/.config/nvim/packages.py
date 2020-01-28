import argparse
from os import system

pack_dir = "~/.config/nvim/pack/foo/start/"
packages = [
    "dracula/vim",
    "elixir-editors/vim-elixir",
    "kballard/vim-fish",
    "k-takata/minpac",
    "rust-lang/rust.vim",
    "sgur/vim-editorconfig",
    "shougo/neosnippet.vim",
    "sjl/badwolf",
    "tommcdo/vim-exchange",
    "tpope/vim-abolish",
    "tpope/vim-commentary",
    "tpope/vim-eunuch",
    "tpope/vim-fugitive",
    "tpope/vim-repeat",
    "tpope/vim-rsi",
    "tpope/vim-surround",
    "tpope/vim-unimpaired",
    "vim-pandoc/vim-pandoc-syntax",
    "arcticicestudio/nord-vim",
    "fatih/vim-go",
    "ledger/vim-ledger",
]


def get_args():
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--dry-run",
        "-d",
        help="only print commands, don't execute",
        action="store_true",
    )
    return ap.parse_args()


def setup():
    args = get_args()
    if args.dry_run:
        run = print
    else:
        run = system
    return run


def pull():
    run = setup()
    for package in packages:
        cmd = "git clone"
        repo_url = "https://github.com/{}.git".format(package)
        repo_name = package.replace("/", "-")
        dest_dir = pack_dir + repo_name
        run(f"{cmd} {repo_url} {dest_dir}")


def main():
    pull()


if __name__ == "__main__":
    main()
