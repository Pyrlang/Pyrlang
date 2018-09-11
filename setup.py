from setuptools import setup, find_packages

setup(name='Pyrlang',
      version='0.7',
      description='Erlang Node implemented in Python 3.5/Gevent/Asyncio',
      author='Erlang Solutions Ltd and S2HC Sweden AB',
      author_email='dmytro.lytovchenko@gmail.com,pyrlang@s2hc.com',
      url='http://pyrlang.readthedocs.io/',
      packages=find_packages(),
      install_requires=['gevent == 1.2.1']
      )
