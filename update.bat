@echo Agora vai

set GIT_PATH="C:\Users\marcus.santos\AppData\Local\Programs\Git\bin\git.exe"
set REPO_PATH="G:\SAHs"
set GITHUB_USERNAME=msuassuna
set GITHUB_PASSWORD=Mss!130183
set GITHUB_REPOSITORY=SAHs

REM Change to the repository directory
cd G:\SAHs

REM Update the GitHub page
C:\Users\marcus.santos\AppData\Local\Programs\Git\bin\git.exe add .
C:\Users\marcus.santos\AppData\Local\Programs\Git\bin\git.exe commit -m "Update GitHub page"
C:\Users\marcus.santos\AppData\Local\Programs\Git\bin\git.exe push https://msuassuna:Mss!130183@github.com/msuassuna/SAHs.git

REM Open Git Bash
start "" "C:\Users\marcus.santos\AppData\Local\Programs\Git\bin\git.exe" bash.exe

exit
