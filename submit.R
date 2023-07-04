library(pdftools)

## Routine for first submission
pdf_subset("paper/manuscript.pdf",pages = 1:3, output = "paper/title_page.pdf")
pdf_subset("paper/manuscript.pdf", pages = 4:25, output = "paper/main_manuscript.pdf")
pdf_subset("paper/manuscript.pdf", pages = 1, output = "paper/conflict_statement.pdf")


## Routine for second sumbission
pdf_subset("revised_manuscript_230703.pdf", pages = 4:32, output = "revised_main_manuscript.pdf")
pdf_subset("response to reviewers.pdf", pages = 1, output = "letter_editor.pdf")
pdf_subset("response to reviewers.pdf", pages = 2:7, output = "response_reviewers.pdf" )
