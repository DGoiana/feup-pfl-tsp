# Travelling Salesman 

## Group Members and Contribution

Members:
  + Rubem Neto   (up202207086) : 50%
  + Diogo Goiana (up202207086) : 50%

All of the work in this project was made equally.

## Shortest Path Implementation

### The solution:

This solution uses a breadth-first search (BFS) strategy to explore all possible paths between the starting city and the ending city while keeping track of the shortest paths found.
It starts with the initial path containing only the starting city and an empty result list. Then, the <code>bfs</code> function recursively starts exploring paths, following these steps:

- Dequeue the current path to explore
- Checks if the last city in the current path is the destination city
- If it is, the path is added to the result list
- If not, the algorithm generates new paths to explore, by appending adjacent cities that have not been visited yet in the current path

After this, the <code>findShortestPaths</code> function filters through all valid paths (distance > 0), calculating their distances and returning only those that match the minimum one.

### Data structures:

We did not use any auxiliar data structure in this implementation.

### Complexity Analysis

+ **Time Complexity**: The time complexity of the algorithm is O(V + E), V being the number of Cities and E being the number of roads.
+ **Space Complexity**: The space complexity is O(V).

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