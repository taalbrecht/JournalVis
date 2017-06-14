@echo off
@set inputFile=%~dp0rawFiles\mod.txt
@set minsup=30
@set thresh=4
@set maxPattern=3
@set gibbsSamplingIterations=1000
@set topicModel=2

cd TopicalPhrases
echo Data preparing...
call win_runDataPreparation.bat %inputFile%

echo Continuous Pattern Mining ... 
call win_runCPM.bat %minsup% %maxPattern% %thresh%

set numTopics=5
set optimizationBurnIn=100
set alpha=2
set optimizationInterval=50
