from setuptools import setup, find_packages

PKGNAME = 'pyrlang'
VERSION = '0.12-alpha'
DESCRIPTION = 'Erlang Node implemented in Python using asyncio'
AUTHOR = 'Erlang Solutions Ltd and S2HC Sweden AB'
AUTHOR_EMAIL = 'dmytro.lytovchenko@gmail.com,pyrlang@s2hc.com'
URL = 'https://github.com/Pyrlang/Pyrlang/'

with open("README.md", "r", encoding='utf-8') as fp:
    LONG_DESCRIPTION = fp.read()

setup(name=PKGNAME,
      version=VERSION,
      description=DESCRIPTION,
      long_description=LONG_DESCRIPTION,
      long_description_content_type="text/markdown",
      author=AUTHOR,
      author_email=AUTHOR_EMAIL,
      url=URL,
      packages=find_packages(),
      install_requires=["pyrlang-term>=1.2"],
      classifiers=[
          "Programming Language :: Python :: 3.7",
          "License :: OSI Approved :: Apache Software License"
      ])
