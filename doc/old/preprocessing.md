# Preprocessing

jabs can preprocess files and expand variables, like autotools:

Variables store: 
<code> <PROJECT_DIR>/.jabs/etc/variables.conf</code>

Format:
<code>(variable1 "value1")</code>

All variables case insensitive and must be set in input files in UPPER CASE

Format:
<code>@VARIABLE1@</code>

Input files, from which generates destination files, locates in project directory and has extension *.input

Ex.:
<code>test.lisp.input</code>
