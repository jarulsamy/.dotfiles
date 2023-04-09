#!/usr/bin/env python3

import configparser
from pathlib import Path


def get_git_username(path=None):
    """Get GitHub username from .gitconfig file."""
    if path is None:
        path = Path(Path.home(), ".gitconfig")

    config = configparser.ConfigParser()
    config.read(path)

    try:
        return config.get("dot-engine", "github-username")
    except configparser.NoOptionError:
        return None
