twizzer
=======
HOW TO COMPILE:

For administrator:
1, administrator needs to compile the program in his home directory. 
2, administrator needs to change access control for the executable file, using command "chmod u+s program", setting up SUID for the program.
3, administrator needs to change access control for his home directory. so that other students can go to his directory to execute the program. You can use "chmod o+r homedirectory"


For students:
When students want to submit twizzes homework or see reviews, he or she needs to go to administrator's home directory to execute the program. ( I think we need a better way to implement this. Going to administrator's home directory whenever students want to do stuff is really tedious and inefficient. Maybe we can make a sysbolic link in each students home directory ? or maybe put our executable program into /usr/local/bin directory ? but this needs root access, which, unfortunately, we do do have.

Basiclly our program is a bunch of function calls.
Some function calls need to verify user's identity. For example, students can not call "setup" function, which is supposed to be called by administrator.

