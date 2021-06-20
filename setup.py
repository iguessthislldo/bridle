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
    ],
)
