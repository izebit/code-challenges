## About
The app has the similar functionality with unix `wc` - [word count](https://en.wikipedia.org/wiki/Wc_(Unix)) program i.e. you can use it to count bytes, symbols, words or lines in a text passed to.  
It's done as part of [code challenge #1](https://codingchallenges.substack.com/p/coding-challenge-1).  

## Usage

The app takes up to 2 parameters:  
```
word-count -[counter types] [path to file]
```

**counter types:**
- `w` - count of words
- `c` - count of bytes
- `l` - count of lines
- `m` - count of symbols


In case if a counter types parameter is absent, default value is `-lwc`.  
The program reads text from console if path to file isn't passed.


## Build

### Prerequisites:
Before working with the project, download and install [Cabal](https://cabal.readthedocs.io/en/stable/index.html) if it's installed on your machine yet.

### Commands:

to build the project, use the following command:  
```bash
cabal build
```

to run the app:  
```bash
cabal run exe:word-count -- -c test.txt
```
