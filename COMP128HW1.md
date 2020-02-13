1. In the starter code, we have declared a double-ended queue (Deque) data structure to hold the list of drawn points. Describe why this is an appropriate choice.

As the gesture should be recorded from the beginning to the end, which means drawn points which constitue the gesture have the order, we should use the double-ended queue data structure to hold the list of drawn points

2. In order to draw the gesture on the CanvasWindow, you will need to implement several mouse event handlers. List each of the methods you will use, and briefly describe what actions you will need to take in order to draw the gesture on the CanvasWindow.

Points, lines, 


3. In your own words, describe each step of the recognition algorithm. For each step, explain why the actions performed on the data are needed.

**Step 1**: First create a template gesture's path for matching the gesture point path such that the path defined by their original M points is defined by N equidistantly spaced points. After that, the candidate gesture and any loaded templates will all have exactly N points. This will allow us to measure the distance from candidate points and template points.
**Step 2**: Rotate Once Based on the “Indicative Angle” defined as the angle formed between the centroid of the gesture and the gesture’s first point. By doing so, I can find the optimal angle much faster and minimize the distance between the gesture and the template points.
**Step 3**: Scale and Translate
**Step 4**: Find the Optimal Angle for the Best Score



4. Please review each of the provided classes that we have given you. For each one, describe their purpose. When might you want to use the functions provided?

>



5. We have given you a partial class decomposition. Can you think of any more classes you might need to store the data involved in this program? In particular think about what the return type of an attempted gesture recognition should be.

>



6. What is still confusing about the algorithm?

