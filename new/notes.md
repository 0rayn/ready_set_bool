## How an 8bit Adder work ?

### how to add two 1 bit numbers:

-> adding two 1 bit numbers can be done using what's called a 'half adder', because it's only work on the current 2 bits and don't care if there is a carry from a previous addition:  
```   
      _______  
A ---|       |--- sum  
     | H.ADD |  
B ---|_______|--- carry  
```
-> for our black box 'H.Add' to work correctly we need to get this output:
```
| A | B | sum | carry |  
| 0 | 0 |  0  |   0   |  
| 1 | 0 |  1  |   0   |  
| 0 | 1 |  1  |   0   |  
| 1 | 1 |  0  |   1   |  
```
-> and we can anylize this truth table like this:  
- sum equal to 1 just if (A and !B) or if (B and !A). BONUS Points if you knew that that will equal to:  
    ```sum = (A and !B) or (B and !A) = A xor B```
- carry equal to 1 only if A and B:  
    ```carry = (A and B)```  

```   
- What our 'H.Adder' looks like when talking about the sum:
      _______  
A ---|--Xor--|--- sum  
     | |     |  
B ---|--     |--- carry 
     |_______| 

- What our 'H.Adder' looks like when talking about the carry:
      _______  
A ---|----   |--- sum  
     |   |   |  
B ---|--AND--|--- carry 
     |_______| 
```
### full adder:

-> Truth table for full adder:

|BIT0|BIT1|PREV_CARRY|CARRY|