#!/usr/bin/env python3
"""
A bunch of utility functions to access GitHub resouces.

Avoids code duplication, many scripts and keybinds utilize this library.
"""

import configparser
import json
import sys
import urllib.request
from datetime import datetime
from pathlib import Path


def get_git_username(path=None):
    """Get GitHub username from .gitconfig file."""
    if path is None:
        path = Path(Path.home(), ".gitconfig")

    config = configparser.ConfigParser()
    config.read(path)

    try:
        return config.get("dotEngine", "username")
    except configparser.NoOptionError:
        return None


def get_repos():
    """Get list of GitHub repositories."""
    SAVE_LOC = Path("/tmp", datetime.now().strftime("clone-autocomplete-%m-%d-%Y.txt"))
    if SAVE_LOC.exists():
        with open(SAVE_LOC, "r") as f:
            repos = f.readlines()
        for i in repos:
            print(i.strip(), end=" ")
        sys.exit(0)

    # Get GitHub username from gitconfig
    username = get_git_username()
    if username is None:
        sys.exit(1)

    ENDPOINT = f"https://api.github.com/users/{username}/repos?per_page=250"

    data = urllib.request.urlopen(ENDPOINT).read()
    js = json.loads(data)

    # Cache results for faster lookups
    with open(SAVE_LOC, "w") as f:
        for repo in js:
            f.write(repo["name"])
            f.write("\n")
            print(repo["name"])


if __name__ == "__main__":
    get_repos()
