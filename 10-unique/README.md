# Uniq

## About
The app has the similar functionality with unix `uniq` - [uniq](https://en.wikipedia.org/wiki/Uniq) program.  
It's done as part of [coding challenge #10](https://codingchallenges.fyi/challenges/challenge-uniq).  

## Usage

<img src="https://raw.githubusercontent.com/izebit/coding-challenges/74db0d657f9eacbf50f21d2e6ea518a85ccd70df/1-word-count/demo/demo.gif" alt="gif">
<br/>
<br/>

**uniq-like** – report or filter out repeated lines in a file 

### Synopsis                                                               
```
uniq-like [ -c | -d | -u | -h ] [ - | input_file ] [output_file]  
```

### Description                                                          
The uniq utility reads the specified input_file comparing adjacent lines, and writes a copy of each unique input line to the output_file.  If input_file is a single dash (‘-’) or absent, the standard input is read. If output_file is absent, standard output is used for output. The second and succeeding copies of identical adjacent input lines are not written.  Repeated lines in the input will not be detected if they are not adjacent, so it may be necessary to sort the files first.
        
The following **options are available**:                         
**-c**, **--count**  
 Precede each output line with the count of the number of times the line occurred in the input, followed by a single space.   
**-d**, **--repeated**  
    Output a single copy of each line that is repeated in the input.  
**-u**, **--unique**                                                        
    Only output lines that are not repeated in the input.          
**-h**, **--help**                                                          
    Print help                                                      

## Development 
### Implementation
**Time complexity** is O(n)  
**Space complexity** is O(1) 

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
cabal run exe:uniq-like -- -u test-files/test.txt
```