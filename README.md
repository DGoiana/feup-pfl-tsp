# p1-pfl

## Group Members and Contribution

Members:
  + Rubem Neto   (up202207086) : 50%
  + Diogo Goiana (up202207086) : 50%

All of the work in this project was made equally.

## Shortest Path Implementation

## Travelling Sales Person (TSP)


### The solution:

The solution builds on an existing method, as described in [R99]. This approach focuses on calculating the minimum cost needed to travel through each city exactly once, minimizing the cost.

The solution involves breaking down the main problem into smaller subproblems. Each subproblem's solution depends on the outcomes of previous subproblems, creating a recursive structure. 

These smaller solutions are stored in a table, which allows for efficient lookup as each subsequent solution is evaluated based on prior results. To ensure accuracy, the boundaries of the table are defined and set before beginning the calculations.

In the presented approach to solving the Traveling Salesperson Problem (TSP), Vertices are typically represented by integers. However, Cities are represented by strings, some adjustments had to be done.

It is also important to note that we have chosen the **last city** to be the starting point for the TSP trasversal.

### Data Structures:

This implementation leverages two Abstract Data Types (ADTs): Set and Table.

+ The **Set ADT**, which holds unique elements, is used to manage the state of each city’s coordinates.
+ The **Table ADT**, a key-value structure, maps the relationship between city states and their respective paths, associating coordinates with travel paths and costs.

The solution utilizes a dynamic programming approach, specifically a higher-order dynamic programming function inspired by the method in [R99]. This function relies on two main components:

+ The **compute** function, which calculates table entries for each index.
+ The **bnds** function, which defines the table's boundaries.

Thanks to lazy evaluation, this implementation only computes the entries necessary for each index, avoiding unnecessary calculations and improving efficiency.

### Complexity Analysis:

+ **Time Complexity**: The time complexity of the algorithm is O(n^2×2^n).
+ **Space Complexity**: The space complexity is O(n×2^n).