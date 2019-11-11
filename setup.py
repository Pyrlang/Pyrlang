from setuptools import setup, find_packages

setup(name='pyrlang',
      version='0.9',
      description='Erlang Node implemented in Python 3.5/Gevent/Asyncio',
      author='Erlang Solutions Ltd and S2HC Sweden AB',
      author_email='dmytro.lytovchenko@gmail.com,pyrlang@s2hc.com',
      url='http://pyrlang.readthedocs.io/',
      packages=find_packages(),
    # The library requires either asyncio or Gevent, you can relax this
    # dependency if Gevent is not desired
      install_requires=[]
      )
