i=1
for filename in `ls`
do
    mv $filename sine1_box$1_$2_$i.wav
    i=$((i+1))
done
