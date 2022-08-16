from setuptools import setup, find_packages


with open("README.md", "r", encoding='utf-8') as fp:
    LONG_DESCRIPTION = fp.read()


setup(name='pyrlang',
      version='0.11-beta1',
      description='Erlang Node implemented in Python using asyncio',
      long_description=LONG_DESCRIPTION,
      long_description_content_type="text/markdown",
      author='Erlang Solutions Ltd and S2HC Sweden AB',
      author_email='dmytro.lytovchenko@gmail.com,pyrlang@s2hc.com',
      url='https://github.com/Pyrlang/Pyrlang/',
      packages=find_packages(),
      install_requires=["pyrlang-term>=1.2"],
      classifiers=[
            "Programming Language :: Python :: 3.7",
            "License :: OSI Approved :: Apache Software License"
      ])
