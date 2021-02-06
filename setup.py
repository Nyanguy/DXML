from setuptools import setup, find_packages

with open('requirements.txt', 'r') as f:
    setup(
        name="dxml", 
        version="1.0.0",
        description="XML editor|generator language written in Python",
        long_description="A declarative xml tree DSL capable of edition and generation written in Python",
        author="Ivan Yakushev",
        maintainer="Ivan Yakushev",
        packages=find_packages(),
        python_requires='>=3.6',
        install_requires = f.readlines(),
    )

