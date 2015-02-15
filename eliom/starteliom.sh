#!/bin/sh

if [ ! -s data ]; then
    ln -s ../data
fi

mkdir -p local/var/log/oco
mkdir -p local/var/run
mkdir -p local/var/data/oco/ocsipersist
# ocsigenserver.opt automatically replace .cma => cmxs in .conf
while true
do
  echo starting ocsigenserver
  echo '=======================' >> local/var/log/oco/std.log
  echo starting ocsigenserver >> local/var/log/oco/std.log
#  nohup ocsigenserver.opt -c oco.conf 2>&1 >> local/var/log/oco/std.log
  nohup ocsigenserver -c oco.conf 2>&1 >> local/var/log/oco/std.log
  echo ocsigenserver exited
  sleep 10
done
