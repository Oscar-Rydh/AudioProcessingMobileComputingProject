i=1
for filename in `ls`
do
    mv $filename sine1_box13_height_$i.wav
    i=$((i+1))
done
