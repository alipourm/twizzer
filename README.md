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


USER CASE:

1, administrator run command "setup", which sets up deadline, buddy system, and twizze homework.
2, after setting up project, students can see twizze by run command "showTwizze".
3, after setting up project, students can submit their twizze homework. Students can submit homework multiply times, in this case, file name should be defined like twizze-1.hs, twizze-2.hs, twizze-3.hs.
4, after deadline, students can not submit their homeworks.
5, after deadline, administrator need to run comand "nextStep" to switch twizze state.
6, after running nextStep command, students can only see their buddies homework so that they can write reviews for them.(seems like we also need to set up dealine for review).
7, students can submit their review in current state. Students also can submit multiple reviews.
8, administrator then run command "nextStep" to go to next step, which students can only see reviews written by their buddies.
9, maybe we can add other settings, for example, administrator may choose to let all students see others homework and reviews.

