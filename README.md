# COL226 Assignment 3

Evaluation and Type Checking

## Testing

### Building

```bash
cd 2019CS10359_A3   #required directory
make        # compiles the lex and yacc files using mllex and mlyacc, then compiles a3.mlb using mlton to generate the executable a3
```

### Running the executable

```bash
./a3 input.txt # To run a3 on the input file input.txt
```

### Removing the sml, signature, description and executable files

```bash
make clean      # To clean the build and runtime files.
```

