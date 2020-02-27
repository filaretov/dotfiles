import git
import typer
import pathlib
from typing import List

app = typer.Typer()


def find_git_repos(path: pathlib.Path, levels: int):
    if levels == 0:
        return []
    repos = []
    dirs = (p for p in path.iterdir() if p.is_dir())
    for p in dirs:
        if p.joinpath(".git").exists():
            repos.append(p)
        else:
            repos += find_git_repos(p, levels - 1)
    return repos


def is_clean(repo: git.Repo):
    if repo.is_dirty():
        err = typer.style("dirty", fg=typer.colors.RED, bold=True)
        typer.echo(f"{repo.working_dir} {err}")
        return False
    if len(repo.untracked_files) != 0:
        err = typer.style("untracked files", fg=typer.colors.RED, bold=True)
        typer.echo(f"{repo.working_dir} {err}: {repo.untracked_files}")
        return False
    return True


@app.command()
def check(top: pathlib.Path):
    for repo_path in find_git_repos(top, 3):
        try:
            repo = git.Repo(repo_path)
            is_clean(repo)
        except git.exc.InvalidGitRepositoryError:
            continue


if __name__ == "__main__":
    app()

