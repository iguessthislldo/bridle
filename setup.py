from setuptools import setup, find_packages

setup(
    packages=find_packages(),
    entry_points={
        'console_scripts': [
            'bridle=bridle.__main__:main',
        ],
    },
    package_data={
        'bridle': [
            'avlidly/idl.lark',
        ],
    },
    install_requires=[
        'lark-parser',
    ],
)
