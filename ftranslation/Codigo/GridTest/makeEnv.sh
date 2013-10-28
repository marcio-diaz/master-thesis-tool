#!/bin/bash

echo
echo -n Removing previous environment. . . .
rm -Rf environment.tar.gz
echo done!

echo -n Creating environment directory . . .
mkdir environment
echo done!

echo -n Copying files. . . . . . . . . . . .
cp -R  drivers                environment
cp -R  sbin                   environment
cp     deployer.py            environment
cp     directoryStructure.py  environment
cp     gnuplotPlots.py        environment
cp     miscFunctions.py       environment
cp     randomHCNFTest.py      environment
cp     remoteTestRunner.py    environment
cp     reports.py             environment
cp     submitter.py           environment
cp     testRunner.py          environment
echo done!

cd environment

echo -n Renaming files . . . . . . . . . . .
mv sbin bin
echo done!

echo -n Fixing Permissions . . . . . . . . .
chmod +x deployer.py
chmod +x testRunner.py
chmod +x remoteTestRunner.py
echo done!

echo -n Compressing environment. . . . . . .
tar czf ../environment.tar.gz *
echo done!

cd ..

echo -n Removing environment directory . . .
rm -Rf environment
echo done!

echo Finished.
echo
