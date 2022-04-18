#!/bin/bash
echo "Run file to run simulations for several scenarios in parallel."
results_folder="/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/results/"
prefix=("covid19school_simulation")
delim=("_")
suffix=(".R")

simulation_folder=("sim_test_parallel")
file_name=$simulation_folder

cd /Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/model_code/
mkdir $results_folder$simulation_folder

scenarios=(1 2)
for i in ${scenarios[@]}
do
  (
  for j in {1..2}
  do
    seed_i=112233*$i+$j
    ( echo $prefix$delim$file_name$delim$i$suffix
    cp covid19school_simulation_aerosol.R  $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$delim$j$suffix
    sed -ie "s/seed_it=.*/seed_it=$seed_i/g" $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$delim$j$suffix
    sed -ie "s/folder='.*'/folder='$simulation_folder'/g" $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$delim$j$suffix
    sed -ie "s/save.image(file=paste0(resultsPath,folder,'.*'))/save.image(file=paste0(resultsPath,folder,'\/$file_name$delim$i$j.RData'))/g" $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$delim$j$suffix
    sed -ie "s/for(scenario in .*)/for(scenario in $i)/g" $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$delim$j$suffix
    Rscript $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$delim$j$suffix) &
  done
  wait
  ) &
done
wait

# simulation_folder=("simMediumC2")
# file_name=("simMediumC2")
#
# cd /Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/model_code/
# mkdir $results_folder$simulation_folder
#
# for i in {1..6}
# do
#   ( echo $prefix$delim$file_name$delim$i$suffix
#   cp covid19school_simulation_aerosol.R  $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$suffix
#   sed -ie "s/folder='.*'/folder='$simulation_folder'/g" $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$suffix
#   sed -ie "s/save.image(file=paste0(resultsPath,folder,'.*'))/save.image(file=paste0(resultsPath,folder,'\/$file_name$delim$i.RData'))/g" $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$suffix
#   sed -ie "s/for(scenario in .*)/for(scenario in $i)/g" $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$suffix
#   Rscript $results_folder$simulation_folder/$prefix$delim$file_name$delim$i$suffix) &
# done
# wait


#
# simulation_folder=("simH1")
# file_name=("simH1")
#
# cd /Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/
# mkdir $results_folder$simulation_folder
#
# for i in {5..6}
# do
#   (sed -ie "s/folder='.*'/folder='$simulation_folder'/g" covid19school_simulation75.R
#   echo $prefix$delim$file_name$delim$i$suffix
#   cp covid19school_simulation75.R  "$prefix$delim$file_name$delim$i$suffix"
#   sed -ie "s/save.image(file=paste0(resultsPath,folder,'.*'))/save.image(file=paste0(resultsPath,folder,'\/$file_name$delim$i.RData'))/g" $prefix$delim$file_name$delim$i$suffix
#   sed -ie "s/for(scenario in .*)/for(scenario in $i)/g" $prefix$delim$file_name$delim$i$suffix
#   Rscript $prefix$delim$file_name$delim$i$suffix) &
# done
# wait
