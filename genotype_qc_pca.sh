#!/bin/bash

# Subset the genotype data for common variants and uncorrelated SNPs (LD < 0.2)

for chr in {1..22}; do
    input=1kg${chr}
    output=pruned/1kg_chr${chr}
    
    plink --bfile ${input} --maf 0.10 --indep-pairwise 500 50 0.2 --out ${output}
    plink --bfile ${input} --extract ${output}.prune.in --make-bed --out ${output}
done

# Merge all PLINK files 

find . -name "*.bim" | grep -e "pruned" > chr.list 
sed -i 's/.bim//g' chr.list 
plink --merge-list chr.list --out 1kg_qc 

# Use the 1KG QC SNPs to subset the cohort datasets and merge with the reference

awk '{print$2}' 1kg_qc.bim > 1kg_qc_snps.txt
plink --bfile cohort_geno --extract 1kg_qc_snps.txt --recode --make-bed --out cohort_pca