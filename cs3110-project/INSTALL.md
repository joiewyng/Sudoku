## Install Packages
```
opam install ounit
opam install ANSITerminal
```

## Play 

To start sudoku game
```
make play
``` 

## Commands 

updating a value at coordinate (row, column): 
```
edit (row,column,value)
```

removing a value at coordinate (row, column): 
```
remove (row,column)
```

gives one hint
```
hint
```

undo last move
```
undo
```

restart current game
```
restart
```

create new game
```
new game
```

get time spent
```
time
```

gives list of commands you can use
```
help
```

exit game
```
quit
```

row is an integer 0-8

column is an integer 0-8

value is an integer 1-9

hints start at 3

mistakes start at 3