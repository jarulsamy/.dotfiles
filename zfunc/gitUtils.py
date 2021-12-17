#!/usr/bin/env python3

from pathlib import Path
import configparser


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
