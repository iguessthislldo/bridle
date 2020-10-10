from pathlib import Path
from setuptools import setup, find_packages

setup(
    packages=find_packages(),
    entry_points={
        'console_scripts': [
            'avidly=avidly.__main__:main',
        ],
    },
    package_data={
        'avidly': [
            'avlidly/idl.lark',
        ],
    },
    install_requires=[
        'lark-parser',
    ],
)
