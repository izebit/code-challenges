## About
The app has the similar functionality with unix `wc` - [word count](https://en.wikipedia.org/wiki/Wc_(Unix)) program i.e. you can use it to count bytes, symbols, words or lines in a text passed to.  
It's done as part of [code challenge #1](https://codingchallenges.substack.com/p/coding-challenge-1).  

## Usage

<img src="https://raw.githubusercontent.com/izebit/code-challenges/74db0d657f9eacbf50f21d2e6ea518a85ccd70df/1-word-count/demo/demo.gif" alt="gif">
<br/>
<br/>

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


## Development 
### Implementation
**Time complexity** is O(n)  
**Space complexity** is O(1) 

The app reads text sequentially symbol by symbol and performs computation based on input symbol. This approach lets reduce RAM consumption. 

### Build

#### Prerequisites:
Before working with the project, download and install [Cabal](https://cabal.readthedocs.io/en/stable/index.html) if it's installed on your machine yet.  
GHC version is >= 9.8.1

#### Commands:

to build the project, use the following command:  
```bash
cabal build
```

to run the app:  
```bash
cabal run exe:word-count -- -c test.txt
```
