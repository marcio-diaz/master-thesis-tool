rm $1/responses/*/*.dat
rm $1/results/*eps
python ./create_extra_spass_data.py $1/responses/ $2
python ./create_extra_spass_graphs.py $1/responses/
mv $1/responses/*.eps $1/results/
