from setuptools import setup, find_packages

setup(name='Pyrlang',
      version='0.0.1',
      description='Python Distribution Utilities',
      author='Erlang Solutions Ltd and S2HC Sweden AB',
      author_email='pyrlang@s2hc.com',
      url='https://www.pyrlang.com/about/not_built_yet/',
      packages=find_packages(),
      install_requires=['gevent == 1.2.1']
      )
