## Purpose of Application

This is an application to help analyse trends of variables interactively.
The app is designed for trend analyses involving binary response variables
                     
## Data frames which will give valid results 

A valid data set has class data.frame.
It will have a column for the dependent variable.
The unique values will be 0 and 1.
It's class will be numeric.
The user will select a 'Y' and an 'X' variable, A tabular and a graphic
representation of the trends will be seen. These can be downloaded.

## Important Features.

- The _**Choose R Object dropdown**_ will identify all R objects in the user's workspace and the user will choose
which data to analyse.

- For continuous variables the user can decide if he/she wants the variable to be
categorized.

- For categorical variables the user can decide if categories need to be combined.
The dropdown for number of combinations can be used to decide the number 
of combinations. For example- if a variable has levels A, B, C, D and E
and if the user wants to combine A and C into one category and
B and D into another category, the user can select 2 as the number of combinations.
2 drop downs will appear in the for_combining_categories tabpanel.

- Both the tabular and graphical representation of the trends can be downloaded 

## More descriptions

### fake_list 
is a dummy list which contains 2 components.
The first component is a vector of numbers from 1 to 50
The second component is a vector of lower case letters",br(),

### fake_matrix 
is a dummy matrix of random numbers distributed Uniformly
between 0 and 1,br(),

### fake_vector 
is a dummy vectpr of numbers from 1 to 10",br(),

### fake_mtcars 
is a dataframe which selects the variables
mpg, cyl, disp, gear from the mtcars data. The dashboard will not 
present results for this data set.", br(),

### new_iris 
is a modification of the iris dataset.
The variable is_setosa_flag is the response variable of interest.
This is a binary variable indicating if the species is setosa.
The dashboard will present results for this data.",br(),

### ucla_data
is a dataset available online from the IDRE website.
The dataset is an example to help understand logistic regression.
The response variable is admit. This indicates if a student is admitted to a university.
The explanatory variables are gre, gpa and rank of undergraduate institution (a factor variable)
