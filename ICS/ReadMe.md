# ICS -- COMP42215 Introduction to Computer Science
*Dr Rob Powell – Course Lecturer*

## Content
- Topic 1 - Python Data Types  
- Topic 2 - Python Data Structures  
- Topic 3 - Control Flow  
- Topic 4 - Functions and Modules  
- Topic 5 - NumPy  
- Topic 6 - Towards Data Science  
- Topic 7 - Towards Economics and Finance with Pandas  
- Topic 8 - Error Checking  
- IO In Python  

## Feedback 
### Overall Mark 72

|  Section 1 | Marks Available | Mark  |
| ---------- | --------------- | ------|
| Q1         | 5               | 5     |
| Q2         | 10              | 10    |
| Q3         | 5               | 5     |
| Q4         | 5               | 4     |
| Q5         | 10              | 5     |
| Q6         | 10              | 4     |
| Subtotal   | 45              | 33    |

**Section1:** It is more efficient to use functions other than append for generating your lists. You could check that the inputs are lists/arrays of numbers, otherwise the functions could throw an error. You are not timing your functions on a range of different input sizes, just the same input multiple times. No discussion of the computational complexity of your functions. The complexity of both functions should be linear. 

|  Section 2 | Marks Available | Mark  |
| ---------- | --------------- | ------|
| Q7         | 5               | 4     |
| Q8         | 10              | 8     |
| Q9         | 5               | 4     |
| Q10        | 15              | 14     |
| Q11        | 10              | 5     |
| Q12        | 10              | 4     |
| Subtotal   | 55              | 39    |

**Section2:** You should consider using a seed to allow you to generate repeatable random data. You should check first that the matrix is 3x3 in Q8, and you don't require the if statements in the loop. You should check the matrix is square before attempting to find the determinant. You are not timing your functions on a range of different input sizes, same problem as Q5. No evidence of research of the algorithm used in np.linalg.det() or its complexity. The recursive algorithm is factorial in complexity, while the algorithm used by np.linalg.det() is cubic at worst.
