# sheet 1
quarto render "TP_1.qmd" --output "TP_1_questions.pdf" --profile exercise -P solution:false
quarto render "TP_1.qmd" --output "TP_1_solutions.pdf" --profile solution
quarto render "TP_1.qmd" --output "TP_1_master.pdf" --profile solution,exercise --execute-params "code_tools.yaml"
