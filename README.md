Requirements:
Operating System: Linux or Unix-like systems.
Components: GHC, System.Random module.


How to install the project:

1)For instructors:
First, you create a directory, let's call it twizze, under your home directory.
Then you need to create a configuration file called cs583.names by default and put it into twizze directory.
Next you need to put twizzer.hs under this directory, and compile it. You will get a executable file called twizzer.
Before you run the program, make sure that you grant executable access control for others.
Finally, you need to add the twizzer executable into /usr/bin so that students can execute the program. Remember this step requires root access on server.


2)For students:
Students do not need to install program. Whenever they want to execute program, they just type twizzer in command line, since this program is already in /usr/bin.
Before students run this program, make sure that /usr/bin directory is in PATH environment variable.


How to run the software:

1)For instructors:
First, go to twizze directory, and execute the program by typing ./twizzer in commnad line.
Then, program will wait for you to type any commands.
You can type "usage" to show you all the available commands.

Availabe commands for instructors:

#setUp deadlineT deadlineR twizzeName
This command is used to set up a new homework, deadlineT stands for twizze deadnline , deadlineR stands for review deadline.
The format for deadline should be like this: 11Jun2013-08:30PM

#checkT
This command is used for instructors to check who has submitted the twizze homework and who hasn't.

#checkR
This command is used for instructors to check who has submitted the review and who hasn't.

#combineT
This command is used for instructors to combine all submitted twizze homeworks into a file. In the case where instructors want
students to see others homework, this commnad is very useful. We have planned to email package in Haskell to automatically deliver
this file to all registered studnets email account.

#combineR
This command is used for instructors to combine all submitted review into a file. Again I wannted to add some code to automatically
deliver this file to all students email accout.


2)For students:
Just type twizzer in your command line.
Then, program will wait for you to give any commands.
You can type "usage" to show all the available commands.
Not all commands are available for students at one time, because it depends on what current state is.

Available commands for students:

#subT filePath
This command is used to submit twizze homework. You can submit twizze multiple times.
However, you can not submit any twizze after the deadline.

#subR filePath
Same functionality as subT, subR is used to submit review for your buddies.

#seeR
This command is used to see reviews written by your buddies. This commands is available only in SeeReview state.

#showT
This command is used to show your buddies homework so that you can write review for them.

#showA
This command is used to show current assignment.


Functionality and scope of the project
1, review is one on one review.
2, If instructors do not have root acces on server, then all students need to make a symbolic link to the executable file in isntrutors directory.
