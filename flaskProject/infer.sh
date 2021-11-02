#!/bin/sh

echo "Starting infer script"
echo datafilename is ${1}
echo modelfilename is ${2}
echo predfilename is ${3}
source mt-cnn/bin/activate
python /home/ncidoesvct2/infer.py ${1} ${2} ${3}
echo "Completed infer script"
exit 0