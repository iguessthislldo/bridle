from setuptools import setup, find_packages

setup(
    packages=find_packages(),
    entry_points={
        'console_scripts': [
            'bridle=bridle.__main__:main',
        ],
    },
    install_requires=[
        'pcpp',
        'pygments>=2.8',
        'rich',
    ],
    extras_require={
        'lsp': [
            'pygls>=0.11',
        ],
    },
)
