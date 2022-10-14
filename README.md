# Harvard Side Porject

## Folder get_eval_embed

This is a folder with codes to:
- Generate Embedding
- Evaluate Embedding
- Get Evaluation Plots

To run it, simply run the script **get_eval_embed.R**. But make sure you have the following files available:
- **MultiAxialHierarchy.csv**
- **AllRelationPairsWithNull.Rdata**
- Input data file, such as **rpdr_code_cooccurrence_victor_2019.csv**

You can tune the dimensions on varaible **dims**, each dimension may take 26-30 mins to run. The evaluation plots are saved as "Summary-XX-YY-ZZ", where XX stands for starting dimension, YY stands for endding dimension and ZZ stands for Step.
