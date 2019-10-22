import argparse
from os import system

packages = [
  'dracula/vim',
  'elixir-editors/vim-elixir',
  'kballard/vim-fish',
  'k-takata/minpac',
  'romainl/flattened',
  'rust-lang/rust.vim',
  'sgur/vim-editorconfig',
  'shougo/neosnippet.vim',
  'sjl/badwolf',
  'tommcdo/vim-exchange',
  'tpope/vim-abolish',
  'tpope/vim-commentary',
  'tpope/vim-eunuch',
  'tpope/vim-fugitive',
  'tpope/vim-repeat',
  'tpope/vim-rsi',
  'tpope/vim-surround',
  'tpope/vim-unimpaired',
  'vim-pandoc/vim-pandoc-syntax',
  ]

def get_args():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", "-d", help="only print commands, don't execute", action="store_true")
    return ap.parse_args()

def setup():
    args = get_args()
    if args.dry_run:
        exec = print
    else:
        exec = system
    return exec

def pull():
    exec = setup()
    for package in packages:
        cmd = "git clone"
        repo_url = "https://github.com/{}.git".format(package)
        repo_name = package.replace("/", "-")
        dest_dir = "~/.config/nvim/pack/foo/" + repo_name
        exec("{} {} {}".format(cmd, repo_url, dest_dir))

def main():
    pull()


if __name__ == "__main__":
    main()
