
mkdir ../log

for LEVEL in class order family genus species 
do
	echo "Running Taxonomy_vs_Feature.R on taxaLevel: " $LEVEL
	Rscript ../*/Taxonomy_vs_Feature.R $LEVEL &> ../log/${LEVEL}.log
done

echo "Done."