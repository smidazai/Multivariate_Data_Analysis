# sheet 1
quarto render "TP_2.qmd" --output "TP_2_questions.pdf" --profile exercise -P solution:false
quarto render "TP_2.qmd" --output "TP_2_solutions.pdf" --profile solution
quarto render "TP_2.qmd" --output "TP_2_master.pdf" --profile solution,exercise --execute-params "code_tools.yaml"
