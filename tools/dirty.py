import git
import typer
from pathlib import Path
from typing import List

app = typer.Typer()


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
def check(repo_paths: List[Path]):
    for repo_path in repo_paths:
        try:
            repo = git.Repo(repo_path)
            is_clean(repo)
        except git.exc.InvalidGitRepositoryError:
            continue


if __name__ == "__main__":
    app()

