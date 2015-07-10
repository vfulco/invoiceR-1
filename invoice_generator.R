library(devtools)
install_github("rpremraj/mailR")

library(mailR)
library(openxlsx)
library(knitr)


price <- c(Milk = 3.50,
			Cream = 6.00,
			Yogurt = 4.00,
			Whey = 2.00,
			MediumEggs = 2.50,
			LargeEggs = 3.50,
			OtherCategory = 0)


spread_sheet <- read.xlsx("purchase_records.xlsx", sheet=1, startRow=1, colNames=TRUE, detectDates=T)




# Here we want to get the names of everyone that purchased something this month.
# Eventually, we'll want to double check that we have an email address for
# everyone and that we haven't introduced a typo into the spreadsheet.

patrons <- levels(factor(spread_sheet$Name))

make_email <- function(full_name, password){
	surname <- gsub(".* (.*)", "\\1", full_name)
	firstname <- gsub("(.*) .*", "\\1", full_name)

	customer <- spread_sheet[spread_sheet$Name==surname,]
	patron <- customer[1,2]
	dates <- customer[,1]
	returns <- customer[,3]
	customer <- customer[,-c(1,2,3)]

	n_entries <- sum(!is.na(customer[,-ncol(customer)]))
	invoice_table <- data.frame(Date=rep("xxx", n_entries),
								Items=rep("xxx", n_entries),
								Quantity=rep(0, n_entries),
								Price=rep(0, n_entries),
								Total=rep(0, n_entries),
								stringsAsFactors=FALSE )

	counter <- 1
	for(visit in 1:nrow(customer)){
		for(category in names(price)){
			if(!is.na(customer[visit,category])){

				invoice_table[counter, "Date"] <- as.character(dates[visit])

				if(category != "OtherCategory"){
					invoice_table[counter, "Items"] <- category
					invoice_table[counter, "Quantity"] <- customer[visit,category]
					invoice_table[counter, "Price"] <- price[category]
					invoice_table[counter, "Total"] <- price[category] * customer[visit,category]
				} else {
					invoice_table[counter, "Items"] <- customer[visit,category]
					invoice_table[counter, "Quantity"] <- 1
					invoice_table[counter, "Price"] <- customer[visit,"OtherPrice"]
					invoice_table[counter, "Total"] <- customer[visit,"OtherPrice"]
				}
				counter <- counter + 1
			}
		}
	}
	invoice_table$Price <- format(invoice_table$Price, nsmall=2L)
	invoice_table$Total <- format(invoice_table$Total, nsmall=2L)

	pretty_table <- kable(invoice_table, align=c('l', 'l', 'c', 'r', 'r'))
	grand_total <- sum(as.numeric(invoice_table$Total))

	text <- paste0("Dear ", firstname, ",<br><br>We appreciate your support of our farm as we strive to provide our community with healthy food while raising our family. Your total for the month of ", months(dates[1]), " is $", format(grand_total, nsmall=2L), ". The details of what items you obtained from Eight Kids Farm are in the table below. You can pay by cash or check the next time you visit. If you have questions about your bill, please email me at sjschloss@gmail.com or call (413)548-4444.<br><br>Sincerely,<br>Sarah Schloss<br><br>")


	write(paste(c(text, "", "", pretty_table), collapse="\n"), "email.Rmd")
	knit2html("email.Rmd", options = "")

	send.mail(from = "sjschloss@gmail.com",
		to = c("pdschloss@gmail.com"),
		subject = "June bill from Eight Kids Farm",
		body = "email.html",
		html = TRUE,
		inline = TRUE,
		smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "sjschloss@gmail.com", passwd = password, ssl = TRUE),
		authenticate = TRUE,
		send = TRUE)

	file.remove(c("email.Rmd", "email.md", "email.html"))
}

print("Please enter your password: ")
password <- readLines(n=1)
patrons <- c("Mike Breslin")
sapply(patrons, make_email, password)


# need to automate date in subject line
# need to pull out date's data from spreadsheet
# need to count milk jars
