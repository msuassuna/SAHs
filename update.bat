@echo off

set GIT_PATH="C:\Users\marcus.santos\AppData\Local\Programs\Git\bin"
set REPO_PATH="G:\SAHs"
set GITHUB_USERNAME=msuassuna
set GITHUB_PASSWORD=Mss!130183
set GITHUB_REPOSITORY=SAHs

REM Change to the repository directory
cd %REPO_PATH%

REM Update the GitHub page
%GIT_PATH% add .
%GIT_PATH% commit -m "Update GitHub page"
%GIT_PATH% push 

REM Open Git Bash
start "" "%GIT_PATH%" bash.exe

exit