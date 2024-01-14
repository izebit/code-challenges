## About
**JSON Parser**  
The command line tool checks correctness of [JSON file](https://www.json.org/json-en.html).  

It's done as part of [coding challenge #2](https://codingchallenges.substack.com/p/coding-challenge-2).  

## Usage

```
✅ ./json-parser ./test-files/success/pass1.json
❌ ./json-parser ./test-files/fail/fail1.json
expected { or [ tokens, but not string token
```

If json file is valid, the tool returns 0, otherwise it prints error message and exit with code 2.

## Development 
### Testing
Tests files were loaded from http://www.json.org/JSON_checker/test.zip 

```bash
cabal test
```

### Build

#### Prerequisites:
Before working with the project, download and install [Cabal](https://cabal.readthedocs.io/en/stable/index.html) if it's installed on your machine yet.  
GHC version is >= 9.8.1

#### Commands:

To build the project, use the following command:  
```bash
cabal build
```

To run the app:  
```bash
cabal run exe:json-parser -- ./test-files/fail/fail1.json
```
